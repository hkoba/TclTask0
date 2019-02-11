#!/usr/bin/env tclsh
# -*- coding: utf-8 -*-

# This code is ispired from make.awk, originally found in:
# http://www.cs.bell-labs.com/cm/cs/awkbook/
# http://www.cs.bell-labs.com/cm/cs/who/bwk/awkcode.txt

package require snit
package require struct::list

snit::type TclTaskRunner {
    option -quiet no
    option -dryrun no
    option -debug 0
    option -debug-fh stdout

    option -known-keys ""; # For user extended keys
    variable myKnownKeysDict []
    typevariable ourRequiredKeysList [set KEYS [list depends action]]
    typevariable ourKnownKeysList [list {*}$KEYS check]

    variable myDeps [dict create]

    variable myWorker ""

    #========================================
    constructor args {
        $self worker install [from args -worker ""]
        
        $self configurelist $args
    }

    #========================================

    method dputs args {$self dputsLevel 1 {*}$args}
    method dputsLevel {level args} {
        if {$options(-debug) < $level} return
        puts $options(-debug-fh) $args
    }

    #========================================

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

    # Shorthand form of target add.
    method add {name depends {action ""} args} {
        $self target add $name depends $depends action $action {*}$args
    }

    #========================================

    method update {name {contextVar ""} args} {
        if {$contextVar ne ""} {
            upvar 1 $contextVar ctx
        } else {
            set ctx [$self context new {*}$args]
        }
	if {![dict exists $myDeps $name]} {
	    return 0
	}

	set changed []
	dict set ctx visited $name 1
	set depends [dict get $myDeps $name depends]
	foreach pred $depends {
	    if {[set v [dict-default [dict get $ctx visited] $pred 0]] == 0} {
		$self update $pred ctx
	    } elseif {$v == 1} {
		error "Task $pred and $name are circularly defined!"
	    }

	    # If predecessor is younger than the target,
	    # target should be refreshed.
	    if {[set prevAge [$self age $pred ctx]]
                < [set thisAge [$self age $name ctx]]} {
		lappend changed $pred
	    } else {
                $self dputs  Not changed $pred prev $prevAge this $thisAge
            }
	}
	dict set ctx visited $name 2

	if {[llength $changed] || [llength $depends] == 0} {
            if {[llength $changed]} {
                $self dputs do action $name because changed=($changed)
            } else {
                $self dputs do action $name because it has no dependencies
            }
	    $self target do action $name ctx
	} else {
            $self dputs not yet updated $name
        }
        if {$contextVar eq ""} {
            set ctx
        }
    }

    method age {name contextVar} {
        upvar 1 $contextVar ctx
        if {[$self context fetch-state ctx $name age]} {
            return $age
        }
        if {[dict exists $myDeps $name check]} {
            $self target do check $name ctx
            if {[$self context fetch-state ctx $name age]} {
                return $age
            } else {
                return Inf
            }
        } else {
            if {[$self file exists $name]} {
                expr {1.0/[$self file mtime $name]}
            } elseif {[dict exists $myDeps $name]} {
                return Inf
            } else {
                error "Unknown node or file: $name"
            }
        }
    }

    method file {cmd args} {
        {*}$myWorker [list file $cmd {*}$args]
    }

    #========================================
   
    method {target list} {} {dict keys $myDeps }
    method names {} { dict keys $myDeps }

    # synonyms of [$self target dependency $target]
    method {target deps} target {$self target dependency $target}
    method {dependency list} target {$self target dependency $target}

    method {target dependency} target {
        dict get $myDeps $target depends
    }

    method forget name {
	if {![dict exists $myDeps $name]} {
	    return 0
	}
	dict unset myDeps $name
	return 1
    }

    #========================================

    method {target do action} {target contextVar} {
        upvar 1 $contextVar ctx
        set script [$self target script-for action $target]
	if {$options(-quiet)} {
            $self dputs target $target script $script
        } else {
            puts $options(-debug-fh) $script
	}
	if {!$options(-dryrun)} {
            set res [$self worker apply-to $target $script]
            $self context set-state ctx $target action $res

            $self dputs ==> $res

            # After action, do check should be called again.
            $self target do check $target ctx
	}
        dict lappend ctx updated $target
    }

    method {target script-for action} target {
        set action [dict get $myDeps $target action]
        set script [if {[dict exists $myDeps $target check]} {
            list ::TclTaskRunner::DO $self \
                check [dict get $myDeps $target check] \
                do $action
        } else {
            set action
        }]
        
        $self script subst $target $script
    }

    method {target do check} {target contextVar} {
        if {[set script [$self target script-for check $target]] eq ""} return
        upvar 1 $contextVar ctx
        
        set resList [$self worker apply-to $target $script]
        $self context set-state ctx $target check $resList
        if {$resList ne ""} {
            set rest [lassign $resList bool]
            if {$bool} {
                $self context set-state ctx $target age [set age [clock microseconds]]
                $self dputs target age is updated: $target age $age
            }
        }
    }

    method {target script-for check} target {
        $self script subst $target \
            [dict-default [dict get $myDeps $target] check ""]
    }

    #========================================
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

    #========================================

    method {context new} args {
        if {[llength $args] % 2 != 0} {
            error "Odd number of context arguments: $args"
        }
        dict create {*}$args \
            visited [dict create] state [dict create] updated []
    }

    method {context set-state} {contextVar target key value} {
        upvar 1 $contextVar ctx
        dict set ctx state $target $key $value
    }
    
    method {context fetch-state} {contextVar target key} {
        upvar 1 $contextVar ctx
        upvar 1 $key result
        if {[dict exists $ctx state $target $key]} {
            set result [dict get $ctx state $target $key]
            return 1
        } else {
            return 0
        }
    }

    #========================================

    method {worker apply-to} {target script} {
        {*}$myWorker [list apply [list {self target} $script] \
                          $self $target]
    }

    method {worker install} worker {
        if {$worker eq ""} {
            set worker [list interp eval {}]
        }

        install myWorker using set worker

        {*}$myWorker [list namespace eval ::TclTaskRunner {}]
        {*}$myWorker [list proc ::TclTaskRunner::DO {self ck check do action} {
            set rc [catch $check __RESULT__]
            if {$rc ni [list 0 2]} {
                return [list no rc $rc error $__RESULT__]
            } elseif {[lindex $__RESULT__ 0]} {
                return yes
            } else {
                eval $action
            }
        }]
    }

    method {script subst} {target script args} {
	set deps [dict get $myDeps $target depends]
        string map [list \
                        \$@ $target \
                        \$< [lindex $deps 0] \
                        \$^ $deps \
                        {*}$args
                       ] $script
    }

    #========================================

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
        
        source [file dirname [info script]]/helper/extras.tcl

        TclTaskRunner dep {*}[TclTaskRunner::parsePosixOpts ::argv]
        if {[llength $::argv]} {
            set ::argv [lassign $::argv fileName]
        } else {
            set fileName TclTask.tcl
        }
        if {![file exists $fileName]} {
            error "Can't find $fileName"
        }
        set realFile [file normalize $fileName]
        cd [file dirname $realFile]
        dep configurelist [TclTaskRunner::parsePosixOpts ::argv]
        if {[dep cget -debug]} {
            puts "sourcing $realFile"
        }
        source $realFile
    }}
}
