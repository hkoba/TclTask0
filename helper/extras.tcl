# -*- mode: tcl; coding: utf-8 -*-

snit::method TclTaskRunner dispatch {argList defaultCommand args} {
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

snit::method TclTaskRunner {method} {name argList body} {
    set map [namespace ensemble configure $self -map]
    set baseArgs [lrange [dict get $map configurelist] 1 end]
    set proc [list ::apply [list [list type selfns win self {*}$argList] \
                                $body] \
                  {*}$baseArgs]
    namespace ensemble configure $self \
        -map [list {*}$map $name $proc]
}

snit::method TclTaskRunner run {cmd args} {
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
