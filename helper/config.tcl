# -*- mode: tcl; coding: utf-8 -*-

#========================================

snit::method $type {config get} {name args} {
    if {![dict exists $myKnownConfig $name]} {
        error "Unknown config: $name"
    }
    if {[dict exists $myActualConfig $name]} {
        dict get $myActualConfig $name
    } elseif {[llength $args]} {
        lindex $args 0
    } elseif {[dict exists $myKnownConfig $name default]} {
        dict get $myKnownConfig $name default
    } else {
    }
}

snit::method $type {config import} args {
    if {$args eq ""} {
        set args [dict keys $myKnownConfig]
    }
    foreach __name__ $args {
        uplevel 1 [list set $__name__ [$self config get $__name__]]
    }
}

snit::method $type {config schema} {} { set myKnownConfig }
snit::method $type {config dump} {} { set myActualConfig }

snit::method $type {config declare} {cfgName {description ""} args} {
    if {[dict exists $myKnownConfig $cfgName]} {
        error "Duplicate config decl: $cfgName"
    }
    set dict [dict create description $description]
    foreach item $args {
        set rest [lassign $item name]
        if {$rest eq ""} {
            dict set dict $name 1
        } else {
            dict set dict $name $rest
        }
    }
    dict set myKnownConfig $cfgName $dict
}

snit::method $type {config source} file {
    if {![file readable $file]} {
        error "Can't read file: $file"
    }
    catch {rename $myConfigReader ""}
    set myConfigReader [interp create]
    interp alias $myConfigReader dep {} $self
    interp eval $myConfigReader [list set self $self]
    foreach name [dict keys $myKnownConfig] {
        interp alias $myConfigReader $name \
            {} dict set myActualConfig $name
    }
    interp eval $myConfigReader [list source $file]
}

