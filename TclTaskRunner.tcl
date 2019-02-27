#!/usr/bin/env tclsh
# -*- coding: utf-8 -*-

# This code is ispired from make.awk, originally found in:
# http://www.cs.bell-labs.com/cm/cs/awkbook/
# http://www.cs.bell-labs.com/cm/cs/who/bwk/awkcode.txt

package require snit
package require struct::list
package require fileutil

namespace eval TclTaskRunner {
    ::variable scriptFn [::fileutil::fullnormalize [info script]]
    ::variable libDir [file dirname $scriptFn]

    source $libDir/helper/util.tcl

    namespace export *
}

snit::macro TclTaskRunner::Macro {} {
    option -quiet no
    option -dryrun no
    option -log-fh stdout
    option -debug 0
    option -debug-fh stdout

    option -known-keys ""; # For user extended keys
    variable myKnownKeysDict [dict create]
    typevariable ourRequiredKeysList [set KEYS [list depends action]]
    typevariable ourKnownKeysList [list {*}$KEYS check]

    option -indent "  "

    variable myDeps [dict create]

    variable myActualConfig [dict create]
    variable myKnownConfig [dict create]
    variable myConfigReader ""

    variable myWorker ""

    #========================================
    constructor args {
        $self worker install [from args -worker ""]

        $self configurelist $args
    }

    method source taskFile {
        if {![file exists $taskFile]} {
            error "Can't find $taskFile"
        }
        set taskFile [file normalize $taskFile]
        pushd_scope prevDir [file dirname $taskFile]
        if {$options(-debug)} {
            puts "sourcing $taskFile"
        }

        $type apply-in-ns $selfns {self type selfns} [list source $taskFile]\
            $self $type $selfns
    }

    #========================================

    method yes {depth args} {$self dputs $depth {*}$args; expr {"yes"}}
    method no {depth args} {$self dputs $depth {*}$args; expr {"no"}}

    method dputs {depth args} {$self dputsLevel 1 $depth {*}$args}
    method dputsLevel {level depth args} {
        if {$options(-debug) < $level} return
        set indent [string repeat $options(-indent) $depth]
        foreach line [split $args \n] {
            puts $options(-debug-fh) "$indent#| $line"
        }
    }

    #========================================

    method {target add} {name args} {
        if {[dict exists $myDeps $name]} {
            error "Task $name is multiply defined!"
        }
        # XXX: [llength $args] == 1 form.
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

    method update {name {contextVar ""} {depth 0} args} {
        if {$contextVar ne ""} {
            # Called from dependency.
            upvar 1 $contextVar ctx
        } else {
            # Root of this update.
            set ctx [$self context new {*}$args]
        }
        if {![dict exists $myDeps $name]} {
            if {$contextVar eq ""} {
                error "Unknown file or target: $name"
            }
            return 0
        }

        dict lappend ctx examined $name

        set changed []
        dict set ctx visited $name 1
        set depends [dict get $myDeps $name depends]
        foreach pred $depends {
            $self dputs $depth $name depends on $pred
            if {[set v [dict-default [dict get $ctx visited] $pred 0]] == 0} {
                $self update $pred ctx [expr {$depth+1}]
            } elseif {$v == 1} {
                error "Task $pred and $name are circularly defined!"
            }

            # If predecessor is younger than the target,
            # target should be refreshed.
            if {[set thisMtime [$self mtime ctx $name $depth]]
                < [set predMtime [$self mtime ctx $pred $depth]]} {
                lappend changed $pred
            } elseif {$predMtime == -Inf && $thisMtime != -Inf} {
                $self dputs $depth Not changed but infinitely old: $pred
                lappend changed $pred
            } else {
                $self dputs $depth Not changed $pred mtime $predMtime $name $thisMtime
            }
        }
        dict set ctx visited $name 2

        if {[if {[llength $changed]} {

            $self yes $depth do action $name because changed=($changed)

        } elseif {[llength $depends] == 0} {

            $self yes $depth do action $name because it has no dependencies

        } elseif {[$self mtime ctx $name $depth] == -Inf} {

            $self yes $depth do action $name because it is infinitely old

        } else {

            $self no $depth No need to update $name

        }]} {

            $self target do action ctx $name $depth
        }
        if {$contextVar eq ""} {
            set ctx
        }
    }

    method mtime {contextVar name depth} {
        upvar 1 $contextVar ctx
        if {[$self context fetch-state ctx $name mtime]} {
            return $mtime
        }
        if {[dict exists $myDeps $name check]} {
            $self target do check ctx $name $depth
            if {[$self context fetch-state ctx $name mtime]} {
                return $mtime
            } else {
                return -Inf
            }
        } else {
            if {[$self file exists $name]} {
                $self file mtime $name
            } elseif {[dict exists $myDeps $name]} {
                return -Inf
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

    method {target do action} {contextVar target depth} {
        upvar 1 $contextVar ctx
        set script [$self target script-for action $target]
        if {$options(-quiet)} {
            $self dputs $depth target $target script $script
        } else {
            puts $options(-log-fh) $script
        }
        if {!$options(-dryrun)} {
            set res [$self worker apply-to $target $script]
            $self context set-state ctx $target action $res

            $self dputs $depth ==> $res

            # After action, do check should be called again.
            $self target do check ctx $target $depth
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

    method {target do check} {contextVar target depth} {
        if {[set script [$self target script-for check $target]] eq ""} return
        upvar 1 $contextVar ctx

        set resList [$self worker apply-to $target $script]
        $self context set-state ctx $target check $resList
        if {$resList ne ""} {
            set rest [lassign $resList bool]
            if {$bool} {
                $self context set-state ctx $target mtime \
                    [set mtime [expr {[clock microseconds]/1000000.0}]]
                $self dputs $depth target mtime is updated: $target mtime $mtime
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
        if {![dict size $myKnownKeysDict]} {
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
        {*}$myWorker [list apply [list {self target} $script $selfns] \
                          $self $target]
    }

    method {worker install} worker {
        if {$worker eq ""} {
            set worker [list interp eval {}]
        }

        install myWorker using set worker

        {*}$myWorker [list namespace eval $type {}]
        {*}$myWorker [list namespace eval $selfns {}]

        # Send the definition of TclTaskRunner::DO to myWorker.
        foreach cmd {DO} {
            set nsCmd ${type}::$cmd
            {*}$myWorker [list proc $nsCmd \
                              [info args $nsCmd] [info body $nsCmd]]
        }
    }

    method {script subst} {target script args} {
        set deps [dict get $myDeps $target depends]
        string map [list \
                        \$@ $target \
                        \$< [string trim [lindex $deps 0]] \
                        \$^ [lrange $deps 0 end] \
                        {*}$args
                       ] $script
    }

    #========================================
    typemethod parsePosixOpts {varName {dict {}}} {
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

    typemethod apply-in-ns {ns varList command args} {
        apply [list $varList $command $ns] {*}$args
    }

    typemethod {helper enable} file {
        set fn $TclTaskRunner::libDir/helper/$file

        $type apply-in-ns :: type [list source $fn] $type
    }

    typemethod toplevel args {
        $type helper enable config.tcl
        $type helper enable extras.tcl

        set self ::dep
        $type $self {*}[$type parsePosixOpts args]\
            -debug [default ::env(DEBUG) 0]

        if {[llength $args]} {
            set args [lassign $args taskFile]
        } else {
            set taskFile TclTask.tcl
        }

        $self configurelist [$type parsePosixOpts args]

        scope_guard args [list set ::argv $::argv]
        set ::argv $args

        $self source $taskFile
    }
}

snit::type TclTaskRunner {
    TclTaskRunner::Macro
}

if {![info level] && [info script] eq $::argv0} {

    TclTaskRunner toplevel {*}$::argv

}
