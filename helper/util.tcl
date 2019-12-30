#!/usr/bin/env tclsh
# -*- coding: utf-8 -*-

proc DO {self ck check do action} {
    set rc [catch $check __RESULT__]
    if {$rc ni [list 0 2]} {
        return [list no rc $rc error $__RESULT__]
    } elseif {[lindex $__RESULT__ 0]} {
        return yes
    } else {
        eval $action
    }
}

proc dict-default {dict key default} {
    if {[dict exists $dict $key]} {
        dict get $dict $key
    } else {
        set default
    }
}

proc default {varName default} {
    upvar 1 $varName var
    if {[info exists var]} {
        set var
    } else {
        set default
    }
}

proc pushd_scope {varName newDir} {
    uplevel 1 [list scope_guard $varName [list cd [pwd]]]
    cd $newDir
}

proc scope_guard {varName command} {
    upvar 1 $varName var
    uplevel 1 [list trace add variable $varName unset \
                   [list apply [list args $command]]]
}

proc configlist obj {
    set configList []
    foreach item [$obj configure] {
        lappend configList [lindex $item 0] [lindex $item end]
    }
    set configList
}
