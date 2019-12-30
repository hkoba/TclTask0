#!/usr/bin/env TclTaskRunner.tcl
# -*- coding: utf-8 -*-

# TclTask.tcl file is sourced in [namespace eval ::],
# from [apply] with some snit-specific arguments ($self, $type and $selfns).


#
# [dep] is a toplevel TclTaskRunner object. You can refere it as $self too.
#
# dep add $TARGET $DEPENDS $ACTION
#
dep add a.o {a.c a.h} {cc -c $< -o $@}

# $ACTION can contain arbitrary tcl script.
# It also supports following Makefile equivalent of shorthand variables:
#
# $@  target
# $<  first element of dependency list
# $^  dependency list

# You can define and use any procedure in tcl.
# Actually, this procedure is defined in $selfns.
# XXX: Currently, proc is not propagated to remote worker.
proc cc args {exec gcc {*}$args}

#
# Again $self refers [dep]. This may help recursive(multi directory) tasks.
#
$self add b.o {b.c b.h} {cc -c $< -o $@}

#
# $self target add $TARGET depends $DEPENDS action $ACTION
#
# is equivalent of:
#
# $self add $TARGET $DEPENDS $ACTION
#

$self target add c.o depends {c.c c.h} action {cc -c $< -o $@}
$self target add main.o depends {main.c a.h b.h c.h} action {cc -c $< -o $@}
$self target add prog depends {main.o a.o b.o c.o} action {cc $^ -o $@}

#
# You can add instance specific methods here like below:
#
$self method hello args {
    # puts "$self $type $selfns"
    puts [list HELLO $args]
    return OK
}

#
# You can also add method to snit::type like below:
#
snit::method $type hello2 args {
    puts [list Another hello $args]
    return OK2
}

if {[$self cget -debug]} {
    puts [list ::argv $::argv]
    
    puts [list targets: [$self target list]]
    
    foreach t [$self target list] {
        puts [list dependency of $t: {*}[$self dependency list $t]]
    }
}

#
# Some of TclTaskRunner methods are implemented in helper/extras.tcl.
#

#
# [$self dispatch ARGSVAR DEFAULT ?PHONY_TARGET COMMAND...?] reads given ARGSVAR
# and dispatches to corresponding target. If no targets are found,
# it will pass all arguments to $self like [$self {*}$::argv], so that
# you can invoke arbitrary methods like [$self info methods]...
#
$self dispatch $::argv {

    $self update prog

} clean {

    $self run file delete {*}[$self target list]

}

# Equivalent code of [$self dispatch ...]:
# 
# if {$::argv eq ""} {
#      $self update prog
# 
# } else {
#     switch [lindex $::argv 0] {
#         clean {
#             file delete {*}[$self target list]
#         }
#         default {
#             puts [$self {*}$::argv]
#         }
#     }
#  }
