#!/usr/bin/env tclsh
# -*- coding: utf-8 -*-

# This code is ispired from make.awk, originally found in:
# http://www.cs.bell-labs.com/cm/cs/awkbook/
# http://www.cs.bell-labs.com/cm/cs/who/bwk/awkcode.txt

package require snit
package require struct::list

snit::type TaskRunner {
    option -quiet no
    option -dryrun no
    option -debug 0
    option -debug-fh stdout

    option -known-keys ""; # For user extended keys
    variable myKnownKeysDict []
    typevariable ourRequiredKeysList [set KEYS [list depends action]]
    typevariable ourKnownKeysList [list {*}$KEYS check age actionRes checkRes]

    variable myDeps [dict create]

    #----------------------------------------
    method dispatch {argList defaultCommand args} {
        if {[llength $args] % 2 != 0} {
            error "Odd number of dispatch extension list!"
        }
        if {$argList eq ""} {
            uplevel 1 $defaultCommand
        } elseif {[dict exists $args [lindex $argList 0]]} {
            uplevel 1 [dict get $args [lindex $argList 0]]
        } else {
            puts [$self {*}$argList]
        }
    }

    # Define instance specific method. This heavyly depends on internals of snit.
    method {method} {name argList body} {
        set map [namespace ensemble configure $self -map]
        set baseArgs [lrange [dict get $map configurelist] 1 end]
        set proc [list ::apply [list [list type selfns win self {*}$argList] \
                                  $body] \
                      {*}$baseArgs]
        namespace ensemble configure $self \
            -map [list {*}$map $name $proc]
    }

    # wrapper for dryrun
    method run {cmd args} {
	if {!$options(-quiet)} {
	    puts $options(-debug-fh) "$cmd $args"
	}
	if {$options(-dryrun)} {
	    return
	}
	if {$cmd eq "self"} {
	    uplevel 1 [list $self {*}$args]
	} else {
	    uplevel 1 [list $cmd {*}$args]
	}
    }

    variable myLogUpdatedList []
    method {loglist updated} {} {
        set myLogUpdatedList
    }

    method {target list} {} {
        dict keys $myDeps
    }

    # synonyms of [$self target dependency $target]
    method {target deps} target {$self target dependency $target}
    method {dependency list} target {$self target dependency $target}

    method {target dependency} target {
        dict get $myDeps $target depends
    }

    # For shorthand
    method add {name depends {action ""} args} {
        $self target add $name depends $depends action $action {*}$args
    }

    method {target add} {name args} {
	if {[dict exists $myDeps $name]} {
	    error "Task $name is multiply defined!"
	}
        set dict [dict create {*}$args]
        if {[set errors [$self task verify $dict]] ne ""} {
            error "Task $name has error: $errors"
        }
	dict set myDeps $name $dict
    }

    method {task verify} dict {
        set errors []
        set missingKeys []
        foreach k $ourRequiredKeysList {
            if {![dict exists $dict $k]} {
                lappend missingKeys $k
            }
        }
        if {$missingKeys ne ""} {
            lappend errors "Mandatory keys are missing: $missingKeys"
        }
        set unknownKeys []
        if {$myKnownKeysDict eq ""} {
            foreach k [list {*}$options(-known-keys) {*}$ourKnownKeysList] {
                dict set myKnownKeysDict $k 1
            }
        }
        foreach k [dict keys $dict] {
            if {![dict exists $myKnownKeysDict $k]} {
                lappend unknownKeys $k
            }
        }
        if {$unknownKeys ne ""} {
            lappend errors "Unknown keys: $unknownKeys"
        }
        set errors
    }

    method update {name {visited ""}} {
        if {$visited eq ""} {
            $self reset
            if {$options(-debug)} {
                puts $options(-debug-fh) [list deps: $myDeps]
            }
        }
	if {![dict exists $myDeps $name]} {
	    return 0
	}

	set changed []
	dict set visited $name 1
	set depends [dict get $myDeps $name depends]
	foreach pred $depends {
	    if {[set v [dict-default $visited $pred 0]] == 0} {
		$self update $pred $visited
	    } elseif {$v == 1} {
		error "Task $pred and $name are circularly defined!"
	    }
	    if {$options(-debug)} {
		set diff [expr {[$self age $name] - [$self age $pred]}]
		puts $options(-debug-fh) "Age diff of target($name)-pred($pred)=($diff)"
	    }
	    # If predecessor is younger than the target,
	    # target should be refreshed.
	    if {[$self age $pred] < [$self age $name]} {
		lappend changed $pred
	    }
	}
	dict set visited $name 2

	if {[llength $changed] || [llength $depends] == 0} {
            if {$options(-debug)} {
                if {[llength $changed]} {
                    puts "do action $name because changed=($changed)"
                } else {
                    puts "do action $name because it has no dependencies"
                }
            }
	    $self target do action $name
	    return 1
	}
	return 0
    }

    method reset {} {
        set myLogUpdatedList []
        foreach target [dict keys $myDeps] {
            dict unset myDeps $target age
        }
    }

    method {target do action} target {
        set script [$self target script-for action $target]
	if {!$options(-quiet)} {
	    puts $options(-debug-fh) $script
	}
	if {!$options(-dryrun)} {
            # XXX: make-lambda?
            apply [list {self target} $script] $self $target

            # After action, do check shoul be called
            $self target do check $target
	}
        lappend myLogUpdatedList $target
    }

    method {target script-for action} target {
        set action [dict get $myDeps $target action]
        set script [if {[dict exists $myDeps $target check]} {
            __EXPAND {
                set rc [catch {@COND@} __RESULT__]
                if {$rc ni [list 0 2]} {
                    return [list no rc $rc error $__RESULT__]
                } elseif {[lindex $__RESULT__ 0]} {
                    return yes
                } else {
                    @ACTION@
                }
            } \
                @COND@ [dict get $myDeps $target check] \
                @ACTION@ $action
        } else {
            set action
        }]
        
        $self target subst-script $target $script
    }

    method {target do check} target {
        if {[set script [$self target script-for check $target]] eq ""} return
        
        set lambda [$self make-lambda $script target $target]
        set resList [{*}$lambda]
        if {$resList ne ""} {
            set rest [lassign $resList bool]
            if {$bool} {
                dict set myDeps $target age [set age [clock microseconds]]
                if {$options(-debug)} {
                    puts $options(-debug-fh) "target age is updated: $target age $age"
                }
            }
            dict set myDeps $target checkRes $rest
        }
    }

    method {target script-for check} target {
        $self target subst-script $target \
            [dict-default [dict get $myDeps $target] check ""]
    }


    method {target subst-script} {target script args} {
	set deps [dict get $myDeps $target depends]
        __EXPAND $script \
            \$@ $target \
            \$< [lindex $deps 0] \
            \$^ $deps \
            {*}$args
    }

    proc __EXPAND {template args} {
	string map $args $template
    }

    method make-lambda {script args} {
        set argVarList [list self]
        set argValueList [list $self]
        foreach {var val} $args {
            lappend argVarList $var
            lappend argValueList $val
        }
        list apply [list $argVarList $script] {*}$argValueList
    }

    method age name {
        if {[dict exists $myDeps $name age]} {
            return [dict get $myDeps $name age]
        }
        if {[dict exists $myDeps $name check]} {
            $self target do check $name
            if {[dict exists $myDeps $name age]} {
                return [dict get $myDeps $name age]
            } else {
                return Inf
            }
        } else {
            if {[file exists $name]} {
                expr {1.0/[file mtime $name]}
            } elseif {[dict exists $myDeps $name]} {
                return Inf
            } else {
                error "Unknown node or file: $name"
            }
        }
    }

    #----------------------------------------
    method forget name {
	if {![dict exists $myDeps $name]} {
	    return 0
	}
	dict unset myDeps $name
	return 1
    }

    method names {} {
	dict keys $myDeps
    }
    #----------------------------------------
    proc dict-default {dict key default} {
	if {[dict exists $dict $key]} {
	    dict get $dict $key
	} else {
	    set default
	}
    }
    proc parsePosixOpts {varName {dict {}}} {
        upvar 1 $varName opts

        for {} {[llength $opts]
                && [regexp {^--?([\w\-]+)(?:(=)(.*))?} [lindex $opts 0] \
                        -> name eq value]} {set opts [lrange $opts 1 end]} {
            if {$eq eq ""} {
                set value 1
            }
            dict set dict -$name $value
        }
        set dict
    }

}


if {![info level] && [info script] eq $::argv0} {
    apply {{} {
        TaskRunner dep {*}[TaskRunner::parsePosixOpts ::argv]
        if {[llength $::argv]} {
            set ::argv [lassign $::argv fileName]
        } else {
            set fileName TclTask.tcl
        }
        if {![file exists $fileName]} {
            error "Can't find $fileName"
        }
        dep configurelist [TaskRunner::parsePosixOpts ::argv]
        if {[dep cget -debug]} {
            puts "sourcing $fileName"
        }
        source $fileName
    }}
}