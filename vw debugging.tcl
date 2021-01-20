#proc (just an aid to my editor for text folding)
# this is version .006 :)
#these array indices, serve as a configuration for this debugger so be careful
#with the other ones, which are used by the debugger

# ---------------------------------
# C O N N F I G U R A T I O N begin
# ---------------------------------

set ::___zz___(proc_wid) 15 		;# the number of lines to show on either side of a breakpoint line
set ::___zz___(auto_list_default) 1 ;# this sets the auto list checkbox to this value at first creation of checkbox
set ::___zz___(console_hack) 1      ;# if 1, installs a console hack to allow an empty <cr> line on console, repeats last command (handy for go+)
set ::___zz___(use_ttk) 0           ;# if 1, the windows use the themed ttk
set ::___zz___(max_size) 3000   	;# the maximum size of a variable, for safety, also if the variable does not yet exist, we can't monitor it
set ::___zz___(skip_modulo) 100    	;# when using a large skip count on go+ this is the number of steps between reporting remaining messages
#set ::___zz___(bwidget) 0 ;# uncomment this if BWidgets are not wanted, leave undefined and it will try to use it (do not set to 1 here)


interp alias {} v {} vw+ ;# shorthands since we might be typing these, optional
interp alias {} g {} go+
interp alias {} u {} util+

# -------------------------------
# C O N N F I G U R A T I O N end
# -------------------------------

#
#these statements are at global level, outside of all procs, and shouldn't be changed (unless you really know what you're doing)

set ::___zz___(lg-skip) [linsert [info global] end ___zz___] ;# skip initial system set of globals, + this one
set ::___zz___(skips) 0 	    ;# the number of breakpoints to skip, set here to avoid an info exist test, do not change, internal use only
set ::___zz___(cb1) 0   		;# the global wide breakpoint disable flag, set it here so we don't have to check for existance later
set ::___zz___(level) 0   		;# 
set ::___zz___(delay) 0   		;# debugging delay times to slow down what's going on
set ::___zz___(goto) -1   		;# debugging goto line number
set ::___zz___(bpnum) 0

set ::___zz___(vw+) "vw+" 		;# the name of the vw+ proc (can perhaps change these if desired, both here and any aliases)
set ::___zz___(go+) "go+" 		;# the name of the go+ proc 
set ::___zz___(bp+) "bp+" 		;# the name of the bp+ proc
set ::___zz___(lbp+) "lbp+" 	;# the name of the lbp+ proc
set ::___zz___(util+) "util+"   ;# the name of the font adjuster, didn't want to use apply, would make the callback look too ugly
 

# reminder to self, get the code out of the step trace debugger (the forerunner to this) which will
# create an entry box in the code window to use as a mini-console, with command history and the enter
# key to repeat, then won't need the console hack, which is not as clean as I'd like it
# maybe, just maybe, pull in the code that would use color to indicate when a variable changed it's value
# between breakpoints. However, that was always slow, so not sure it's really worth it. Time to update pdf
# and take a rest.
# .006 
# added the g -N command (triggered also by a double click on a line number), an animated go to a line
# where you can see the line number moving rapidly and variables updating quickly, however, it does not
# yet verify you are still in the same proc, so go to line -30, and if you have instrumented another proc
# and you call it from the first, it will stop on that line number w/o checking it's the same proc as where
# you issued the command (a todo item)
#
# cleaned up the breakpoint number so that g N (positive value) can be more reliably used to implement a
# with a program restart to get to the same point, (or just before it) assuming code repeats exactly, 
# implements a sort of run backwards ability :)
# 
# added a leave trace as well where the -bg of the text widget is set to a shade of yellow, and on the entry it's back to white
# fixed some quoting difficulties, had to use our global array to copy some difficult text into the namespace for local variables
# then we can copy the locals using a variable instead of trying to add backslashes all over the place, don't like it, but it seems
# to be working. Glad this is still a local thing on github, nobody is using this yet besides me.
#
# added an entry trace on the instrumented proc so we can remove any previous namespace copy of the local variables
# and -revert doesn't have to delete the trace, since revert redefines the proc and that removes any traces
# a subsequent re-instrument will put it back
# if the user calls instrument+ but doesn't do the eval, we won't get a new trace, and we'll insist on another revert, eh that's no biggy 
#
#proc wait { ms } { ;# a wait for use to debug the debugger
#	set uniq [incr ::__sleep__tmp__counter]
#	set ::__sleep__tmp__$uniq 0
#	after $ms set ::__sleep__tmp__$uniq 1
#	vwait ::__sleep__tmp__$uniq
#	unset ::__sleep__tmp__$uniq
#}
#   --------------------------------------------------------------------
#   This is a collection of procedures to be sourced into a new program
#   They make up a simple debugging system for global
#   and namespace static variables and proc local dynamic variables.
#
#   Usage:
#       1. setup the several configuration options below, if desired
#       2. add a [source] of this file at the beginning of a program
#       3. place breakpoints in source code
#          note, code outside procs use bp+ and inside procs lbp+, OO methods use only bp+
#       4. optionally add single stepping using
#          eval [instrument+ procname ?options?] 
#                - will redefine procname with added calls to lbp+
#                -norb -> don't add breakpoints after right braces
#                -revert -> change back to before instrumentation
#                 (don't run the instrument more than once, it only saves the last one)
#                                        
#       5. run program, at first breakpoint, several windows will open
#       6. ... debug program ...
#
#   There's 1 command to view variables, vw+ which can be called directly or by the
#   other commands to display variables. It creates view windows for global variables
#   and namespace variables. Its also used by lbp+ to show local variables
#    
#   There are 3 commands for breakpoints, bp+ lbp+, and go+. The bp+ and lbp+ commands
#   are added into code where a breakpoint is desired. The go+ command will resume
#   a program. It can be entered from a console or by a button in a vw+ window. There's also
#   1 helper routine to change fontsize in the code viewer window, 
#   plus it has a proc listing command (util+ lp <procname>)
#
#   The console "command repeat" is an optional configuration - an empty line and <return> repeat the
#   last command. Handy in the console to repeat the go+ command by just pressing <return>
#   multiple times (w/o needing to do an up arrow first) it also suppresses the console history
#   count when repeating. This little mod is from the wiki under console.
#
#   A total of 6 global commands and 1 global array variable (for configuration and control). The
#   global variable is purposely obscure, ::___zz___     The 6 names can be configured
#   and then changed to anything else ( you don't have to change their proc statements, 
#   if they use a variable for the name)
#
#   These user commands can be imbedded in code or typed into a console,
#   
#   vw+ - an expanded version of RS's little global variable view tool
#        The support for array variables shows (in column 2) only the
#        indices of the array (not the data), in a readonly entry widget
#        
#        It allows multiple windows using one window per namespace 
#        
#        Each label in column 1 (the variable name) is left clickable and will
#        write the variable value to stdout; if an (global) array it writes with [parray]
#
#        Each window has 2 buttons, go = will continue from
#        a breakpoint if paused, refresh = update array variables indices
#        with current set of idices for this one window. At a breakpoint
#        all array variables in all vw+ windows will be refreshed, while
#        regular variables are automatically refreshed when they change value
#
#        If the command is run again, it will rewrite the variables and if a pattern
#        is given, it will get a new current set of variables. The refresh button
#        can be clicked with the shift key down and it will redo the same command
#        that had been entered previously. In addition, the size and position of
#        the window will be restored if possible.
#
#   bp+ - breakpoint, uses vwait - will also try to refresh all vw+ windows array variable indices
#        This takes on optional parameter, a message to output when the command is run
#        these are placed in the program, not set with a command like a regular debugger
#        if used inside a proc, will not show local variables or code, but will stop, see also lbp+
#
#   lbp+ - breakpoint used ONLY inside of a proc, supports local variables which are copied to
#         a namespace and then displayed using vw+ Also can list on stdout a snipit
#         of code surrounding the breakpoint. It also takes an optional message, and
#         an identifier (something unique, used to find the current line in code listing)
#         if there's no identifier, uses the message, but that might not be unique
#
#   go+ n - continue from a breakpoint, by setting the vwait-ed variable, this is also
#        called by vw+ windows if you click the go button. The optional nnumber arg n is to skip 
#        n breakpoints - note while it skips, it runs slowly, better to put a check in the code
#        and only execute the breakpoint (bp+ or lbp+) after that many steps - slow because it
#        will still update all the variables on each step, turning off bp reporting helps a bit
#
#   
#   One global variable is used, ___zz___ which is an array and has
#   several elements. A few notables:
#   
#   ___zz___(lg-skip) is used to snapshot the initial set of global variables
#   (does this one time before the other procs are defined) this is
#   used by vw and lg to work only on globals the user has set after 
#   startup when some 15-20 system globals are defined,
#   vw+ * will overide athat however, and display all globals
#   
#   ___zz___(bp) the array element vwait uses in bp+ and go+ 
#   
#   ___zz___(bpnum) counter of breakpoints reached
#
#   ___zz___(vws) list of open windows to refresh on next breakpoint
#
#    The viewer program vw+ can be executed interactively or from the program. It
#    creates 1 or more windows with lists of variables to monitor. It can be called with a
#    window name, which defaults to .vw and can be called multiple times. If it includes
#    a window name not already in use, it will create another window. If a window exists
#    using that name, it will be changed to reflect the pattern and variables that match. 
#
#   Some examples of usage for vw+
#
#   vw+ ?        ;# display short help text   
#   vw+          ;# default all parameters, view only "user" globals, using .vw window and wid=80
#   vw+ *        ;# view all global variables using .vw and 80
#   vw+ t        ;# view all global variables matching t*
#   vw+ {[te]}   ;# view all global variables matching [te]* must brace it however, no spaces unless escaped
#   vw+ . . 100  ;# view user globals(default), using default .vw window, but set max width to 100
#   vw+ {foo bar baz}   ;# monitor a specific list of global variables, with window/width defaulted
#   vw+ e errors 120    ;# monitor e* variables, use .errors for window, width=120                           
#   vw+ *  .all         ;# all globals, use another toplevel window .all, default width
#   vw+ *  all          ;# ditto ^ leave off the . on the window parameter and it will be added in front
#                       ;# while using the default for width
#                       ;# if .all is already open, this will replace it with
#                       ;# a new window and possibly a different set of variables
#
#   Example use with namespace variables:
#
#   namespace eval myname {
#           variable foo bar  
#           variable x 1.23  
#           variable xbot
#           set xbot(abc) something
#   }
#
#   vw+ myname::    ;# will setup a window for namespace and all its variables, leading :: optional
#   vw+ myname::x   ;# will only track variables matching myname::x*
#                   ;# note the pattern always has a * appended
#                   ;# if there is a :: anywhere in the pattern, that
#                   ;# triggers it to use [info vars] rather than [info glob]
#                   ;# arrays in namespaces work also, where you use [variable] on just the array
#                   ;# name and then afterwards use set or array to assign values to elements
#
#   Array variables show only the current set of indices, and the entry box is set
#   to readonly (which nicely highlights it). They will be refreshed only at a breakpoint
#   but there is a refresh button on each window that can be used to update the list of incices.
#   The entry widget with the array indices can be selected for copying, but cannot be modified.
#   
#   In a vw+ window, there are 4 bindings on the label (the variable name)
#      left-click         -> display the variable's value on stdout, if an array, use parray
#      shift-left-click   -> if an array, use parray, if a list, try to display as a dict
#      control-left-click -> output the list and display in a column on stdout (console)
#      alt-left-click -> sort the list and display in a column on stdout (console)
#   ----------------------------------------------------------------------


proc $::___zz___(vw+) {{pat {**}}  {w .vw} {wid 80} {alist {}}} {

	set me $::___zz___(vw+) ;# name of this procedure, used for recursion, callbacks, and help
	set go $::___zz___(go+) ;# name of the go from breakpoint command, used in a callback
#	puts stderr "at the toppat= |$pat| wid= |$wid| w= |$w| alist length = [llength $alist]" ; update
	if { $pat eq "?" } {
		puts "$me pattern   window   width   - patterns are \[string match\] type"
		puts "$me {a list}  window   width   - alternate form, with list of >1 variable names"
		puts "   pattern   * => all globals   ** => only user globals (the default)"
		puts "             text  => text* can also use glob with \[abc\] etc. * always added to end "
		puts "             foo:: => foo::* vars, not globals but namespace variables only"
		puts "   width     width of entry widget with variable data, defaults to 80"
		puts "   window    default to .vw, can use several windows at same time"
		puts "   {a list}  a list of specific variables, can be undefined"
		puts ""
		puts "   Any parameter can be a . = shorthand for default, none are required"
		puts "   Note: the patten parameter cannot look like a list of > 1 element"
		puts "         it must be in {}'s to use the \[ab\] string pattern "
		puts ""
		puts "   On first call to $me, BWidget's is loaded if possible to support scrolling"
		puts "   if it's not available, will fall back to a single window which could be too large"
		puts "   "
		puts "   Left click on Variable:  Shift: dict  Control: List   alt: Sorted List"
		return
	}
	if { $::___zz___(console_hack) == 1} {
		set ::___zz___(console_hack) 2 ;# so we only do this once, first time through
		console eval {
			namespace eval tk { ; # replace this so we can capture a null command and repeat the last one
				proc ::tk::ConsoleInvoke {args} {
					set ranges [.console tag ranges input]
					set cmd ""
					if {[llength $ranges]} {
						set pos 0
						while {[lindex $ranges $pos] ne ""} {
							set start [lindex $ranges $pos]
							set end [lindex $ranges [incr pos]]
							append cmd [.console get $start $end]
							incr pos
						}
					}
					if {$cmd eq ""} {
						ConsolePrompt
					} elseif {[info complete $cmd]} {
						if { $cmd == "\n" } { #patch
							set cmd_next [consoleinterp eval {history nextid}]
							set cmd_event [consoleinterp eval "history event [expr {( $cmd_next - 1 )}]"]
							if { $cmd_event != "" } {
								set cmd $cmd_event
								consoleinterp eval {namespace eval ::tcl {incr history(nextid) -1;incr history(oldest) -1}}  ;# don't store this one again into history
							}
						}
						#end patch
						.console mark set output end
						.console tag delete input
						set result [consoleinterp record $cmd]
						if {$result ne ""} {
							puts $result
						}
						ConsoleHistory reset
						ConsolePrompt
					} else {
						ConsolePrompt partial
					}
					.console yview -pickplace insert
				}
			}
		}
	}
	if { [llength $pat] > 1 } {
		return [$me . $w $wid $pat] ;# allow list to be in first parameter, we recurse with list at the end
	}
	set alist0 $alist ;# remember how we were called for later refresh
#	puts "alist0= |$alist0| "
	if { ! [info exist ::___zz___(bwidget)] } { ;# if variable does not exist, try to use it
		if [catch {
			package require BWidget
			set ::___zz___(bwidget) 1
		} err_code] {
			puts stderr "Turning off scrolling: $err_code"
			set ::___zz___(bwidget) 0
		}
	} else {
#		puts "bwidgets variable exists and is $::___zz___(bwidget)"
	}
	if { $pat eq "-" } { ;#refresh array entries index values, internal call via recursion
#		puts "doing the refresh    pat= |$pat| wid= |$wid| w= |$w| alist= |$alist| "
		set windows [winfo children $w]
		set k [string length $w]
		incr k
#		puts "windows=  |$windows| "
#		puts "windows=s |[lsort $windows]| "
		foreach window $windows {
			set kind [string index $window $k]
			set wnum [string range $window $k+1 end]
#			puts "window= |$window| k = $k  kind = $kind wnum= |$wnum| "
			if { $kind eq "l" } { ;# this is the label on the left, we derive the entry on right
				set var [$window cget -text]
				if { [string range $var end-1 end] eq "()"} {
					set avar [string range $var 0 end-2]
					set ent "${w}.e$wnum" ;# the corresponding entry widget
					set indices [lsort -dictionary [array names ::$avar]] ;# sort the array indices to put into entry
#					puts " its a label = window= |$window| var=$var   avar=$avar ent = $ent  indices -> $indices"
					$ent configure -state normal  ;# make it writeable temporarily
					$ent delete 0 end
					$ent insert 0 $indices
					$ent configure -state readonly  ;# and back to read only
				}
			}
		}
		return
	}
	if { $pat eq "." } {
		set pat "**"
	}
	if { $wid eq "." } {
		set wid 80
	}
	if { $w eq "." } {
		set w ".vw"
	}
#	puts "check for window without a leading . and add it to $w"
	if { [string index $w 0] ne "." } {
		set w .$w ;# allow user to leave off the leading .
#		puts "new w is /$w/"
	}
	if { ![info exist ::___zz___(vws)] || $w ni  $::___zz___(vws)} {
		lappend ::___zz___(vws) $w ;# keep a list of possible windows to refresh
	}
	if { $pat eq "**" || $alist ne ""} {
#		puts "pat= |$pat| " ; update
		if { $alist ne "" } {
			set a $alist
		} else {
			set a [lsort -dictionary [info global ${pat}*]]
		}
#		puts "a= |$a| " ; update
		set argsn [list]
		foreach gvar $a {
			if { $gvar in $::___zz___(lg-skip)  && $pat eq "**" && [llength $alist] == 0} { ;# no pattern given, use only user globals
#				puts ".... $gvar"
				continue
			}
			if {[array exists ::$gvar]} { ;# it is an array get some indices only
#				puts "array $gvar"
				set val "() [lsort -dictionary [array names ::$gvar]]"
				lappend argsn [list $gvar $val]
			} elseif { [info exists ::${gvar}] } {
				lappend argsn [list $gvar {}]
			} else {
#				puts "none $gvar"
				lappend argsn [list $gvar {}] ;# variable doesn't exist yet, treat as non-array, only occurs with user provided list
			}
		}
	} elseif { [llength $pat] == 1 } {
#		puts "in the elseif"
		if { [string match "*::*" $pat] } { ;# if we have :: then it's a namespace lookup
			if { [string range $pat end-2 end] ne "::"} {
				set pat ${pat}:: ;# if there are any :: in it, we need to have :: following, if they are not there, we'll add them
			}
			set alist [lsort [info var ${pat}*]]
		} else {
			set alist [lsort [info glob ${pat}*]]
		}
#		puts "pat is one arg only, pat = |$pat|  alist= |$alist| "
		if { [llength $alist] >= 1 } {
#			puts stderr "-----recursion----------"
			$me $pat $w $wid $alist ;# call ourselves with the new manual list provided, if at least 1
		} else {
#			puts stderr "No globals match ${pat}*"
			error "No globals match ${pat}*"
		}
		return
	} else {
		should-not-happen
	}
#	puts "argsn= |$argsn| " ; update

	set ww .[lindex [split $w .] 1] ;# top level in case it .a.b.c we want just .a
	if { [info exist ::___zz___(vws,$ww)] } { ;# if this exists, then the window existed at some time
		set g [lindex $::___zz___(vws,$ww)  end]
		set gg [split $g +]
		set oldgeom +[lindex $gg 1]+[lindex $gg 2]
		set reincarnated 1 ;# later we'll use this oldgeom for the position, but use the new size that is computed
#		puts "::___zz___(vws,$ww)= |$::___zz___(vws,$ww)| g= |$g| gg= |$gg| oldgeom= |$oldgeom| reincarnated= |$reincarnated| "
	} else {
#		puts "first time for this window: $ww"
		set reincarnated 0
	}

# now we try to keep most of the window, just delete all the variables 
# and rebuild them, faster than destroying toplevel and starting over completely

	set exists [expr {(   [info command $w] ne ""   )}]

	if { ! $exists } {	
		if { $::___zz___(bwidget) } {
	#		package require BWidget, this was done already, but this is the bwidget setup
			toplevel $w
			set sw  [ScrolledWindow $w.sw -relief sunken -borderwidth 2]
			set sff [ScrollableFrame $w.sw.f]
			$sw setwidget $sff
			bind $w <MouseWheel> "$w.sw.f yview scroll \[expr {-%D/60}\] units"
			pack $sw -fill both  -expand yes
			set w [$sff getframe]
#			puts "bwidget container= w= |$w| "
		} else {
			toplevel $w
		}
		
	} else {
		if { $::___zz___(bwidget) } {
			set w $w.sw.f.frame
		} else {
			# w is ok if there's no bwidgets
		}
#		puts "not bwidgets w= |$w| "
	}
	
	if { ! $exists } {	
		frame $w.f1 -relief groove  -padx 5
		frame $w.f2 -relief groove -background green
		grid $w.f1 $w.f2
#		puts "about to create refresh button, use these args: pat= |$pat| w= |$w| wid= |$wid| "
		if { $::___zz___(use_ttk) } {
			set ttk "ttk::"
		} else {
			set ttk ""
		}
		${ttk}button      $w.f1.b1   -text "Refresh" 	-command [list $me - $w 0] ;# refresh the arrays, variables no problem
#		${ttk}button      $w.f1.b1   -text "Refresh" 	-command [list $me $pat $w $wid] ;# refresh the arrays by re-issue vw command new maybe
		set ww .[lindex [split $w .] 1]
		if { $pat ne "**" } {
			set cmd "$me {$pat} {$ww} {$wid} {}" ;# this will cause a re check of the variables based on a pattern
		} else {
			set cmd "$me {$pat} {$ww} {$wid} {$alist0}" ;# default pattern, so use the given list
		}
#		puts "pat= |$pat| ww= |$ww| wid= |$wid| cmd= |$cmd| alist length then a0 len [llength $alist] [llength $alist0]"
		bind  $w.f1.b1 <Shift-1>  [list eval $cmd] ;# shift click on refresh will rebuild with more/less variables
		
		${ttk}button      $w.f1.b2   -text "Go" 		-command [list $go -1 $w] ;# go from breakpoint, remember the current window for refresh
		
		if { $reincarnated == 0 } {
			if { ! [info exist ::___zz___(cb1)] } {
				after 100 "set ::___zz___(cb1) 0" ;# global wide breakpoints disable, all windows use same, 1 check sets all checks
			}
#			puts "checking for (cb2,$ww)"
			if { ! [info exist ::___zz___(cb2,$ww)] } { ;# window may pre-exist, keep value, this one is for the automatic code listing on stdout
				set ::___zz___(cb2,$ww)  $::___zz___(auto_list_default) ;# got to set it now, so we use it now
				after 100 "set ::___zz___(cb2,$ww)  $::___zz___(auto_list_default)" ;#but again so the window manager has time to run
			}
			
			if { ! [info exist ::___zz___(cb3,$ww)] } { ;# window may pre-exist, keep value, this one is for the automatic code listing on stdout
				set ::___zz___(cb3,$ww)  0 ;# got to set it now, so we can use it below
				after 100 "set ::___zz___(cb3,$ww) 0" ;# for local breakpoints, we include the window name so this is window specific breakpoint disabling
			}
			if { ! [info exist ::___zz___(cb4,$ww)] } { ;# window may pre-exist, 
				set ::___zz___(cb4,$ww)  0 ;# got to set it now, so we can use it below
				after 100 "set ::___zz___(cb4,$ww) 0" ;# this disables the messages about stopping at a breakpoint, with user message, and then continuing
			}
			if { ! [info exist ::___zz___(cb5,$ww)] } { ;# window may pre-exist, 
				set ::___zz___(cb5,$ww)  0 ;# got to set it now, so we can use it below
				after 100 "set ::___zz___(cb5,$ww) 0" ;# shows experimental instrumentation code
			}
			if { ! [info exist ::___zz___(cb6,$ww)] } { ;# window may pre-exist, 
				set ::___zz___(cb6,$ww)  0 ;# got to set it now, so we can use it below
				after 100 "set ::___zz___(cb6,$ww) 0" ;# unused checkbox
			}
		}
		# now that we set the checkbuttons variables before creating the checkbuttons, the wm timing seems to not be an issue any longer
		# before that sometimes the checkbuttons were not set with their correct values
		${ttk}checkbutton $w.f2.cb1 -text "Disable all BPs   " -variable ::___zz___(cb1)
		${ttk}checkbutton $w.f2.cb2 -text "Auto-list         " -variable ::___zz___(cb2,$ww)
		${ttk}checkbutton $w.f2.cb3 -text "Disable local BPs " -variable ::___zz___(cb3,$ww)
		${ttk}checkbutton $w.f2.cb4 -text "No BP messages    " -variable ::___zz___(cb4,$ww)
		${ttk}checkbutton $w.f2.cb5 -text "Show Instr-ed Code" -variable ::___zz___(cb5,$ww)
		${ttk}checkbutton $w.f2.cb6 -text "spare             " -variable ::___zz___(cb6,$ww)
		grid $w.f2.cb1  $w.f2.cb2  $w.f2.cb3 $w.f2.cb4  $w.f2.cb5   $w.f2.cb6 ;# options in column 2
		grid $w.f1.b2 $w.f1.b1
	} else {
#		puts stderr "does exist $w, so let's see what we do have, next does the destroys"
#		vwait ::fff
		set children [winfo children $w]
		set got1 1
		set the_ns ""
		foreach child $children {
#			puts "child= |$child|  [winfo class $child] should maybe destroy him  = [expr {[winfo class $child] ne "Frame"}]"
			if { [winfo class $child] ne "Frame" } { ;# kill all the labels and entries, but take the canoli - uh I mean keep the buttons etc.
				set splitwidget [split $child .]
				if { $got1 } {
					set got1 0
#					puts "destroy child= |$child|  /::[lindex $splitwidget 1]/  $splitwidget"
					set the_ns [lindex $splitwidget 1]
				}
#				wait $::___zz___(delay)
				destroy $child
			}	
		}
		# need to delete the namespace here
#		puts "w= |$w|  about to delete -----   $the_ns   -------------"
#		namespace delete $the_ns

		
		
		
		
		
		
#		puts "(not) waiting at fff again"
#		vwait ::fff
	}
	
	set size {{consolas} 12}
	set maxwid 16 ;# compute max variable length so groove fills, but set minimum first
	if { [llength $argsn] == 0 } {
		puts stderr "no matching variables found for pattern $pat"
	}
	foreach ii [lsort -dictionary $argsn] {
		set i [lindex $ii 0]
		set j [lindex $ii 1]
		set len [string length $i]
		if { [string range $j 0 1] eq "()" } {
			incr len 2
		}
		if { $maxwid < $len } {
			set maxwid $len
		}
#		puts "i= |$i| maxwid= |$maxwid| "
	}
	
	set n 0
	foreach ii [lsort -dictionary $argsn] {
#		wait $::___zz___(delay)
		set i [lindex $ii 0]
		set j [lindex $ii 1]
#		puts " n=$n   ii= [format %30s |$ii|] i= |$i| j= |$j|" ; update ; wait 5000
#		puts "   string range = [string range $j 0 1]"
		set zok 1
		set zerror ""
		if { [string range $j 0 1] eq "()" } {
			label $w.l$n -text "${i}()" -anchor w  -font "$size" -bd 1 -width $maxwid -relief groove
#			puts "no entry for i= |$i| j= |$j| ii= |$ii| "
			
			entry $w.e$n  -bg white -width $wid -font "$size"
			$w.e$n insert end [lrange $j 1 end]
			$w.e$n  configure -state readonly
#			puts "just set $w.e$n"
		} else {
#			puts ".. entry for $i"
			
			set sizemax $::___zz___(max_size)
			set sanity -1
			set sanityd "???"
			if [catch {
				set it $i
				if { ! [string match "*::*" $it] } { ;# we need to access it as global from here, so add the ::
					set it "::$it"
#					puts "new it = $it"
				}
#				set sanity [eval "string length \$$it"]
				set sanity [string length [set $it]]
#				set sanityd [eval "string range  \$$it 0 $sizemax"]
				set sanityd [string range [set $it] 0 $sizemax]
				if { $sanity > $sizemax } {
					set splitup [split $it "::"]
					set nspace  [lindex $splitup  2 ]
					set nname  __$nspace
					set fname "::${nspace}::${nname}" ;# check for our proc name, it's the namespace name used twice with extra __
#					puts "it= |$it| sanity= |$sanity| ii= |$ii| i= |$i| j= |$j| splitup= |$splitup| nspace= |$nspace| nname= |$nname|  fname= |$fname| "
					set zok 0
					set zerror "Too large to safely monitor : $sanity  = $sanityd"
					if { $it eq $fname } { ;# if it's ours, we'll be less cautious and allow for a longer string, since it's the entire proc code
						if { $sanity < ($sizemax * 10) } {
							set zok 1	;# so if it's the same as $it it's the enemy who is us
						}
					}
				} else {
					set zok 1	
				}
			} err_code] {
				set zerror  "$err_code"
				set zok 0
			}
			label $w.l$n -text $i -anchor w  -font "$size"  -bd 1 -width $maxwid -relief groove
			if { ! $zok  } {
#				puts "sanity= |$sanity| size= |$sizemax| sanityd= |$sanityd| " ; update
#				continue
				entry $w.e$n   -bg LightYellow1 -width $wid -font "$size" ;# no text variable for this one
				$w.e$n insert end $zerror
			} else {
				entry $w.e$n -textvariable $i -bg white -width $wid -font "$size"
			}
#			entry $w.e$n -textvariable $i -bg white -width $wid -font "$size"
		}


		if { $zok } {
			bind $w.l$n <1> {apply [list {win} {
#					puts "You clicked into window $win"
					set foofoo [$win cget -text]
					if { [string range $foofoo end-1 end] eq "()" } {
						puts stderr "\n---------\nThe array $foofoo\n---------"
						if [catch {
							parray ::[string range $foofoo 0 end-2]
						} err_code] {
							puts $err_code 
						}
					} else {
						puts stderr "\n------------\nThe variable $foofoo\n------------"
						puts "$foofoo = [set ::$foofoo]"
					}
				} ] %W}
			bind $w.l$n <Shift-1> {apply [list {win} {
#					puts "You shift clicked into window $win"
					set foofoo [$win cget -text]
					if { [string range $foofoo end-1 end] eq "()" } {
						puts stderr "\n---------\nThe array $foofoo\n---------"
						parray ::[string range $foofoo 0 end-2]
					} else {
						puts stderr "\n--------------\nThe Dictionary $foofoo\n--------------"
						set llen [llength [set ::$foofoo]]
						set ok 0
						if [catch {
							set foo2 [set ::$foofoo]
#							puts "foofoo= |$foofoo| foo2= |$foo2| llen= |$llen| "
							set dlen [dict size $foo2]
							incr dlen $dlen
							if { $dlen != $llen } {
								error "not a valid dictionary:\nAre there duplicates? \[llength\]= $llen vs. \[dict size\]*2 = $dlen"
								set ok 0
							} else {
#								puts "seems to be valid"
								set ok 1
							}
						} err_code] {
							puts "dictionary error: $err_code"
							set ok 0
						}
						if { $ok } {
#							puts "format as a dict"
							set max 0
#							puts "max= |$max| "
							dict for {key val} $foo2 {
								set len [string length $key] 
								if { $len > $max } {
									set max $len
								}
							}
#							puts "max= |$max| "
							set fstring "  %%-${max}s  =>  |%%s|" 
							dict for {key val} $foo2 {
								puts [format $fstring $key $val]
#								puts "key= |$key| val= |$val| fstring= |$fstring| max= |$max| "
							}
						} else {
#							puts "$foofoo = [set ::$foofoo]"
						}
						
					}
				} ] %W}
			bind $w.l$n <Control-1> {apply [list {win} {
#					puts "You control clicked into window $win"
					set foofoo [$win cget -text]
					if { [string range $foofoo end-1 end] eq "()" } {
						puts stderr "\n---------\nThe array $foofoo\n---------"
						parray ::[string range $foofoo 0 end-2]
					} else {
						set llen [llength [set ::$foofoo]]
						puts stderr "\n--------\nThe List $foofoo   llength: $llen\n--------"
						set ok 1
						if [catch {
							set foo2 [set ::$foofoo]
						} err_code] {
							puts "list error: $err_code"
							set ok 0
						}
						if { $ok } {
#							puts "format as a list"
							set n -1
							set fstring "  %%-4d  =>  |%%s|" 
							foreach val $foo2 { ;# don't sort here
								puts [format $fstring [incr n] $val]	
							}
#							puts "max= |$max| "
						} else {
#							puts "$foofoo = [set ::$foofoo]"
						}
					}
				} ] %W}
			bind $w.l$n <Alt-1> {apply [list {win} {
#					puts "You control clicked into window $win"
					set foofoo [$win cget -text]
					if { [string range $foofoo end-1 end] eq "()" } {
						puts stderr "\n---------\nThe array $foofoo\n---------"
						parray ::[string range $foofoo 0 end-2]
					} else {
						set llen [llength [set ::$foofoo]]
						puts stderr "\n--------\nThe List $foofoo  (sorted -dictionary) llength: $llen\n--------"
						set ok 1
						if [catch {
							set foo2 [set ::$foofoo]
						} err_code] {
							puts "list error: $err_code"
							set ok 0
						}
						if { $ok } {
#							puts "format as a list"
							set n -1
							set fstring "  %%-4d  =>  |%%s|" 
							foreach val [lsort -dictionary $foo2] { ;# alt is for a sorted list
								puts [format $fstring [incr n] $val]	
							}
#							puts "max= |$max| "
						} else {
#							puts "$foofoo = [set ::$foofoo]"
						}
					}
				} ] %W}
		}
			
			
#		puts "grid    	$w.l$n        $w.e$n"
		grid $w.l$n $w.e$n
		incr n
	}
	update ;# wonder if this is needed here
#	puts "w= |$w| "
	if {! $::___zz___(bwidget)} {
		set ww [lindex [split [wm geom $w] +] 0]
		if { $reincarnated } {
			set newgeom $oldgeom
#			puts "newgeom= |$newgeom|  oldgeom= |$oldgeom| "
		} else {
			set newgeom +-6+1
		}
#		puts "w= |$w| ww= |$ww|  newgeom= |$newgeom| "
		wm geom $w $ww$newgeom
	} else {
#		resize the toplevel window so to not need scrollbars if possible, but no bigger than these maxes
		set height 	[expr {(    min(  65 + ($n * 27) , 950 )                    )}] ;# no more than 950
		set width  	[expr {(    min( int( ($wid + $maxwid) * 11.4) +12 , 1600)    )}] ;# no more than 1600
		if { $reincarnated } {
			set newgeom ${width}x$height$oldgeom
#			puts "newgeom= |$newgeom| width= |$width| height= |$height| oldgeom= |$oldgeom| "
		} else {
			if [catch {
				set nws [llength $::___zz___(vws) ]
				incr nws -1
			} err_code] {
				set nws 0 
			}
			set nws [expr {(    $nws % 10   )}] ;# after this many new windows, we start placing them at the top again
			set ycord [expr {(   -6 + ($nws * 100)   )}]
			set newgeom ${width}x$height+-6+$ycord
#			puts stderr "newgeom= |$newgeom| "
		}
		set top [split $w .]
		set wtop .[lindex $top 1]
#		puts "for our window $w we should now try to resize it, wid= $wid  maxwid = $maxwid   n= $n the number of variables"
#		puts "top= |$top| height = $height width = $width"
#		puts "wtop= |$wtop| newgeom= $newgeom skips = $::___zz___(skips) "
		if { $::___zz___(skips) <=0 || $reincarnated == 0} {
			catch {wm geom $wtop $newgeom}
		}
		
	}
#	puts "got here vw 7 l/argsn [llength $argsn]" ; update


	if [catch {
		set wl [split $w .]
		set ww .[lindex $wl 1]
#		puts "w= |$w| pat= |$pat| wid= |$wid| alist= |$alist| wl = |$wl|  ww=|$ww|"
		set ::___zz___(vws,$ww) [list $ww $pat $wid $alist [wm geom $ww]]
#		set ::___zz___(vws,$ww) [list $ww $pat $wid {} [wm geom $ww]]
		bind $ww <Configure> {
			if { [llength [split %W .]] == 2 } {
#				puts "hi from %W -> [wm geom %W] == $::___zz___(vws,%W)"
				lset ::___zz___(vws,%W) end [wm geom %W] ;# update to the current position and size
			} else {
#				puts "hi from %W -> not at the top level now"
			}
		}
	} err_code] {
		puts " Cannot set the vws,$w : $err_code "
	}
	
	return
#	flush stdout ; update
} ;# addapted from the original idea by RS
#$::___zz___(bp+)
proc bp+ {{message {*}}  {nobreak 0}  {nomessage 0} } { ;# the 2nd, 3rd, passed in from lbp+ from the windows checkbox options
#	puts stderr "goto vs. line :  $::___zz___(goto)   [expr {(    $::___zz___(lbp+,line) +1   )}]"
	set stophere 0
	if { $::___zz___(goto) >= 0 } {
		if { [expr {(   $::___zz___(lbp+,line) + 1    )}] ==  $::___zz___(goto) } {
			set stophere 1	
			set ::___zz___(goto) -1	
		}
	} else {
		set stophere 1
	}
	if { $::___zz___(level) > 0} {
		puts stderr "no recursive breakpoints allowed, ignoring, level = $::___zz___(level)"
		return
	}
	if { $::___zz___(cb1) || $nobreak} {
		incr ::___zz___(bpnum)
		return
	}
	if { ![info exist ::___zz___(vws)] } {
		set ::___zz___(vws) [list]
	}
	if {   ($::___zz___(skips)  <= 0)   ||  (  ( $::___zz___(skips)% $::___zz___(skip_modulo) ) == 0    )       } {
#		puts "mod skip_modulo, so do nothing here = $::___zz___(skips) " ;# used to be a constant 50, now a configuration variable
	} else {
#		puts "mod skip_modulo get out quick       = $::___zz___(skips) "
		incr ::___zz___(skips) -1
		if { ! $nomessage } {
			if { $::___zz___(skips) > 0 } {
				set taco "skips remaining : $::___zz___(skips)"
			} else {
				set taco ""
			}
			puts stderr "BReakpoint   [expr {(   $::___zz___(bpnum) + 1   )}]  :  $message"
			update
		}
		incr ::___zz___(bpnum)
		return
	}
	foreach vwindow $::___zz___(vws) {
		if { [info command $vwindow] ne "" } { 
#			puts "try to refresh vwindow= |$vwindow| "
			if { [info exist ::___zz___(bwidget)] && $::___zz___(bwidget) == 1} {
				catch {$vwindow.sw.f.frame.f1.b1 invoke}
#				puts "invoke $vwindow.sw.f.frame.f1.b1"
			} else {
				catch {$vwindow.f1.b1 invoke}
#				puts "invoke $vwindow.f1.b1"
			}
		} else {
			puts "delete vwindow= |$vwindow| "
			set n [lsearch -exact $::___zz___(vws) $vwindow] ;# it has to be there, but we don't know where if some were deleted
		    set ::___zz___(vws) [lreplace $::___zz___(vws) $n $n]
		}	
	}
	if { ! $nomessage } { #; even though we might not actually breakpoint here, we might still send a message, works as a trace
		if { $::___zz___(skips) > 0 } {
			set taco "skips remaining : $::___zz___(skips)"
		} else {
			set taco ""	
		}
		puts stderr "Breakpoint  [incr ::___zz___(bpnum)] :  $message $taco "
		update 
#		flush stdout ; update ;# needed here?
	} else {
		incr ::___zz___(bpnum)
	}
	if { $nomessage } {
		set ::___zz___(bp) 2 ;# indicate we are waiting for it to change but don't want
	} else {
		set ::___zz___(bp) 0 ;# indicate we are waiting for it to change
	}
	if { $::___zz___(skips) > 0 } {
		incr ::___zz___(skips) -1
		set nobreak 1
	}
#	puts stderr "line is [expr {(   $::___zz___(lbp+,line) + 1   )}]"
	if { ! $nobreak && $stophere} {
		incr ::___zz___(level)
		vwait ::___zz___(bp) ;# pause until this is set again
		incr ::___zz___(level) -1
	}
	if { ! $nomessage &&  ! $nobreak} { ;# if we didn't pause, then we don't say continue
		puts stderr "Continuing..."
	}
}
proc $::___zz___(go+) {{skip -1} {window ""}} {
	if { $skip < -1 } {
		set ::___zz___(goto) [expr {(   0 - $skip   )}]
		set skip -1
	}
	if { $skip > 0 } {
		set ::___zz___(skips) [expr {(   $skip - 1   )}]
	}
	set ::___zz___(go-window) $window ;# not really using this anymore, but lets keep it anyway
	if {![info exists ::___zz___(bp)] ||  ($::___zz___(bp) == 1) } {
		puts stderr "Not currently waiting at a breakpoint after $::___zz___(bpnum) steps"
		set ::___zz___(bp) 1 ;# set it regardless
	} elseif {![info exists ::___zz___(bp)] ||  ($::___zz___(bp) == 2)} {
		set ::___zz___(bp) 2 ;# set it so we continue
		return ""
	}	
	set ::___zz___(bp) 1 ;# set it regardless
	return ""
}
proc $::___zz___(util+) {func args} { ;# increase or decrease font, and do the list proc as sub commands
#	puts "func= |$func| args= |$args| "
	if       { $func eq "fontsize" } {
		set w [lindex $args 0 ]
		set dir [lindex $args 1 ]
		set f  [$w cget -font]
		set font  [lindex $f 0]
		set size  [lindex $f 1]
		if { $dir > 0 } {
			incr size
			if { $size > 25 } {
				set size 25
			}
		} else {
			incr size -1
			if { $size < 6 } {
				set size 6
			}
			
		}
		$w config -font "$font $size"
	} elseif { $func eq "double-click" } { 	;# this is used at the end of a proc
#		puts "binding doubleclick text : args= |$args| "
		set selranges [.lbp_console.cframe.text tag ranges sel]
#		puts "selranges= |$selranges| "
		set selection [.lbp_console.cframe.text get {*}$selranges]
#		puts "selranges= |$selranges| selection= |$selection| " 
		if { [string is integer $selection] } {
			tailcall $::___zz___(go+) "-[expr {(   abs($selection)   )}]"
			return
		} 
		puts stderr "Invalid double click selecton, not a number: $selection"
		return
	} elseif { $func eq "tracerend" } { 	;# this is used at the end of a proc
#		puts stderr "in tracerend args= |$args| "
		if [catch {
					.lbp_console.cframe.text configure -bg {#ffffc0}
		} err_code] {
			puts "setting to yellow: $err_code "
		}
	 	return
	} elseif { $func eq "tracer" } { 	;# this is used to clear the namespace for the proc, 
										;# clearing vars so next time in we start over, internal call only
		if [catch {
					.lbp_console.cframe.text configure -bg white
		} err_code] {
#			puts "setting to white: $err_code "
		}
#		puts stderr "in tracer args= |$args| "
		set prc [lindex  $args 0 0] ;# get the proc name from the trace input
		set zzz [namespace exist _$prc] ;# first time a proc is called there's no namespace to clear up
		after 300 [list wm title .lbp_console $prc]
#		puts stderr "zzz= |$zzz| args= |$args| prc= |$prc| "
		if { $zzz } {
#			puts before-wait-1000
#			wait 1000
#			puts stderr "do-namespace-delete _$prc "
			if [catch {
				namespace delete _$prc
			} err_code] {
				puts $err_code 
			}
#			puts after-delete
#			wait 1000
#			puts after-wait-1000
		}
	
	} elseif { $func eq "names" } { #
		set  ns  [lindex $args 0]
		set  var [lindex $args 1]
		puts "lookup in namespace $ns, the var $var"
		return [namespace eval $ns [list namespace which -variable $var]]
	} elseif { $func eq "delay" } { #set the delay factor for debugging
		puts stderr "set delay to [lindex $args 0]"
		set ::___zz___(delay) [lindex $args 0]
	} elseif { $func eq "smod" } { #set the skip modulo
		puts stderr "set skip modulo to [lindex $args 0]"
		set ::___zz___(skip_modulo) [lindex $args 0]
	} elseif { $func eq "clean" } { #close all vw+ windows, from the ___zz___(vws) list
		foreach window $::___zz___(vws) {
			puts "close window= |$window| "
			destroy	$window
		}
	} elseif { $func eq "kill" } { 
	} elseif { $func eq "lp" } {
		# was proc  lp {{namepat *}} # list procedure(s)
		set namepat [lindex $args 0]
		if { $namepat eq "" } {
			error "wrong number of args: should be $::___zz___(util+) lp procedure-name"
		}
if { 00 } {
		foreach proc [info procs $namepat] {
			set space ""
			puts -nonewline "#---------------------\nproc $proc {"
				foreach arg [info args $proc] {
					if [info default $proc $arg value] {
						puts -nonewline "$space{$arg $value}"
					} else {
						puts -nonewline $space$arg
					}
					set space " "
				}
				# No newline needed because info body may return a
				# value that starts with a newline
				puts -nonewline "} {"
				puts -nonewline  [info body $proc]
			puts "}"
		}
}
		
			# let's use the functional version of this	, leaving my orginal above	
			set proc [lindex $args 0]
		
			set result ""
			set space ""
			set result "proc $proc \{"
			foreach arg [info args $proc] {
				if [info default $proc $arg value] {
					append result "$space{$arg $value}"
				} else {
					append result $space$arg
				}
				set space " "
			}
			append result "\} \{"
			append result [info body $proc]
			append result "\}\n"
			return $result
		

	} elseif { $func eq "?" } {
		puts "util+ help: "
		puts "     lp <procedure>       display the current code for a proc "
		puts "     smod    #            set the modula for reporting on skiping (now $::___zz___(skip_modulo))"
		puts "     clean                close all the data windows #= [llength $::___zz___(vws)]"
#		puts "     util+ help: "
#		puts "     util+ help: "
#		puts "     util+ help: "
	} elseif { $func eq "stuff" } {
		dothis-stuff
	} else {
		error "invalid util+ function, should be one of lp, fontsize, smod, clean, ... or ?"
	}
}
#$::___zz___(lbp+)
proc lbp+ { {comment {}} {bpid {}} } { ;# breakpoint from within a proc, will create a window with local vars, id optional
	if { $::___zz___(cb1) } { ;# get out quickly if no breakponts, also don't update values
		incr ::___zz___(bpnum)
		return
	}
	if {   ($::___zz___(skips)  <= 0)   ||  (  ( $::___zz___(skips)% $::___zz___(skip_modulo) ) == 0    )       } {
#		puts "MOD skip_modulo, so do nothing here = $::___zz___(skips) "
	} else {
#		puts "MOD skip_modulo get out quick       = $::___zz___(skips) "
		incr ::___zz___(skips) -1
		incr ::___zz___(bpnum)
		return
	}

	set level [info frame] ;# we need to go up a level or so to get the variables, we copy them to a namespace
	incr level -1
	set frm_dict  [info frame  $level ] 
#	puts "frm_dict= |$frm_dict| "
	set proc_name ""
	set zzz [dict exists $frm_dict proc]
	if { $zzz == 0 } {
		puts "this is not a proc"
		set zzz [dict exists $frm_dict method]
		if { $zzz } {
			puts "its a method, not a proc"
		} else {
			puts "its neither a method or a proc"	
		}
		return
	}
	set proc_name [lindex [dict get $frm_dict proc] 0]
#	puts "proc_name= |$proc_name| frm_dict= |$frm_dict| level= |$level| " ; update
	set ns [string map {{::} {_}} $proc_name]
#	puts "ns= |$ns| proc_name= |$proc_name| "
	if { [info exist ::___zz___(cb5,.$ns)] } {
		set show_instr $::___zz___(cb5,.$ns)
	} else {
		set show_instr 0	
	}
	if { [info exist ::___zz___(cb3,.$ns)]  && $::___zz___(cb3,.$ns)} {
		incr ::___zz___(bpnum)
		return
	}
	
	set vars [uplevel 1 {info vars}]
#	puts "vars= |$vars| "

# need some functions here, but don't want to polute the command name space any more
	
#	get_proc_code $proc_name
	set proc_def [apply {proc {
	
			set result ""
			set space ""
			set result "proc $proc \{"
			foreach arg [info args $proc] {
				if [info default $proc $arg value] {
					append result "$space{$arg $value}"
				} else {
					append result $space$arg
				}
				set space " "
			}
			append result "\} \{"
			append result [info body $proc]
			append result "\}\n"
			return $result
			
	}} $proc_name]
	
#   numberit {pdef string} 
	set search_id $comment
	if { $bpid ne "" } {
		set search_id $bpid
	}
	set ::___zz___(lbp+,line) -1
	set proc_def [apply {{pdef string message show_instr} {
	
			set lines [split $pdef "\n"]
			set num 0
			set out ""
			set next_one 0
			foreach line $lines {
				set cur "  "
				if { $next_one && [string trim $line] ne ""} {
					set cur "->"
					set next_one 0
				}
				if { $string ne "" && [string match  *${string}* $line] } {
					set cur "--"
					set ::___zz___(lbp+,line)  [expr {(    $num + 1   )}]
					if { $message eq "step-instrument" } {
						set next_one 1
						set cur "  "
					}
				}
				if { ! $show_instr } {
					set zzz 0
					set zzz [regsub  {\;lbp\+ step\-instrument.*$} $line "" line]
#					puts "zzz= |$zzz| line= |$line| " 
				}
				append out "${cur}[format %4d [incr num]]\t$line\n"	
			}
			return $out
	
	
	}} $proc_def $search_id $comment $show_instr]
	
#	puts "::___zz___(lbp+,line)= |$::___zz___(lbp+,line)| (cb2,.$ns)"
	
#	puts "proc_def= |$proc_def| "
	
	set pdef "variable __$ns \{\n$proc_def \n \}\n" ;# not a problem here with quoting, since it's a valid proc we got back, so quoting should be correct
	
#	puts "pdef= |$pdef| "
	set ncmd ""
	set ncmd $pdef
#	puts "------------ top ----------------"
	set nvar -1
	foreach var $vars {
		incr nvar
#		puts "var= |$var| "
		set cmd "array exist $var" ;# command to run in caller stack frame
		set arr [uplevel 1 $cmd]   ;# and now run it there
		if [catch {
			set cmd "if \{ ! \[ array exist $var \] \} \{ set $var \} else  \{ lsort \[ array names $var\]  \}     "
			set aval [uplevel 1 $cmd]
			set ok 1
		} err_code] {
			puts "$err_code for $var, so skip it, is it a global and not defined yet?"
			set ok 0
		}
		if { ! $ok } {
			continue
		}
#		puts "var=$var  aval= |$aval| arr= |$arr|"
		
#		set f1 "\{"
#		set f2 "\}"
#		set aval [string map  [list $f1 \\$f1 $f2 \\$f2 ] $aval]
		
		
		set ::___zz___(temp,$nvar) $aval
		if { $arr } {
			append	ncmd "variable $var ()\\ \$::___zz___(temp,$nvar) \n"
		} else {
			append	ncmd "variable $var \$::___zz___(temp,$nvar) \n"
		}
	}
#	puts "in top ncmd= |$ncmd| ns= |$ns| " ;# these are the commands we run to setup the namespace with the user variables
	
#	puts "abouut to do the namespace"
#	puts $ncmd
if [catch {
		namespace eval $ns $ncmd
} err_code] {
	puts $err_code
	puts stderr $ncmd 
}
	$::___zz___(vw+) "${ns}::" .$ns

#	puts $err_code 
#	puts $ncmd
#	vwait ::forever

	
	if { [info exist ::___zz___(cb2,.$ns)] && $::___zz___(cb2,.$ns) && [info exist ::___zz___(cb3,.$ns)] && !$::___zz___(cb3,.$ns) } {
#		puts "do auto list"

#	show_simple $proc_def	$::___zz___(lbp+,line)

	if { [info command .lbp_console] eq ""} {
		
		set font {Consolas 12}
		toplevel .lbp_console
		frame .lbp_console.bframe
		frame .lbp_console.cframe
		text  .lbp_console.cframe.text -height 25 -wrap none -font $font -tabs "[expr {4 * [font measure $font 0]}] left" -tabstyle wordprocessor -width 24 -yscrollcommand [list .lbp_console.cframe.y set]
		scrollbar .lbp_console.cframe.y -orient vertical -command [list  .lbp_console.cframe.text yview]
		
		button .lbp_console.bframe.b1    -text "Clear" -command {.lbp_console.cframe.text delete 1.0 end} ;# -image $image ;#
		button .lbp_console.bframe.b2    -text "Bottom" -command {.lbp_console.cframe.text see end; .lbp_console.cframe.text mark set insert end} ;# -image $image ;#
		button .lbp_console.bframe.b3    -text "Font --" -command [list $::___zz___(util+) fontsize .lbp_console.cframe.text -1] ;# -image $image ;#
		button .lbp_console.bframe.b4    -text "Font ++" -command [list $::___zz___(util+) fontsize .lbp_console.cframe.text 1] ;# -image $image ;#
		button .lbp_console.bframe.b5    -text "Console" -command {catch {console show}} ;# -image $image ;#
		button .lbp_console.bframe.b6    -text "Break" -command {set ::___zz___(skips) 1;set ___zz___(goto) -1} ;# -image $image ;#
		button .lbp_console.bframe.b7    -text "Go" -command [list $::___zz___(go+)]  ;# -image $image ;#
		

		
		
		pack .lbp_console.bframe  -side top -expand 0 -fill x
		pack .lbp_console.cframe 	-side top -expand 1 -fill both
		pack .lbp_console.cframe.text    -side left -expand 1 -fill both
		bind .lbp_console.cframe.text  <Double-Button-1> [list after idle [list $::___zz___(util+) double-click %W %x %y]]
#		bind .t   <Double-Button-1> [list after idle [list $foo double-click %W %x %y]]		
		pack .lbp_console.cframe.y   -side right -expand 0 -fill y
		
		pack  .lbp_console.bframe.b1 .lbp_console.bframe.b2 .lbp_console.bframe.b3  .lbp_console.bframe.b4 .lbp_console.bframe.b5 .lbp_console.bframe.b6  .lbp_console.bframe.b7  -fill both -expand true -side left
		if { [info exist ::___zz___(console_geom) ]} {
			after 100 {wm geom .lbp_console {*}$::___zz___(console_geom) ; update}
				puts "setting console geom $::___settings___(geomcon) "
		} else {
			wm geom .lbp_console 1061x772+-1+185
		}

	} else {
		if { $::___zz___(skips) <= 0 } {
			.lbp_console.cframe.text delete 1.0 end
		}
	}

# output a range of lines around the current line
	set no_result [apply {{code line} {
		if {  $::___zz___(skips) <= 1} {
			set wid $::___zz___(proc_wid)
			set lines [split $code \n]
			set nlines [llength $lines]
			incr nlines -1
			set from [expr {(   $line - $wid   )}]
			set to   [expr {(   $line + $wid   )}]
			set wid2 [expr {(   $wid + $wid   )}]
			
#			puts stderr "from= |$from| line= |$line|  to= |$to| nlines = $nlines  wid= |$wid| wid2= |$wid2| "
			
			if { $nlines <= $wid2 } {
				set from 0
				set to $nlines
			} else {
				if { $from <= 0 } {
#					puts "from= |$from| to= |$to| "
					set to  [expr {(   $to - $from  +1 )}]
					set from 1
#					puts "from= |$from| to= |$to| after incr"
					
				} elseif {$to > $nlines} {
				    incr from  -[expr {(   $to - $nlines   )}] 
				    set to $nlines
				}	
			}

#			puts stderr "from= |$from| line= |$line|  to= |$to| total = $nlines  - final"  
			set num 0
				foreach line $lines {
					incr num
					if { $num < $from || $num > $to } {
						continue
					}
#					puts "${line}"
						.lbp_console.cframe.text insert end-1c "$line\n"
						.lbp_console.cframe.text see end-1c
				}
		}
	
	}} $proc_def $::___zz___(lbp+,line)]

		
	}
	if { $comment eq "" } {
		set colon ""
	} else {
		set colon ": "	
	}
#	puts "ns= |$ns| ::___zz___(cb2,.$ns)= |$::___zz___(cb2,.$ns)| info=|[info command .$ns]|"
	$::___zz___(bp+) "$ns $colon$comment" $::___zz___(cb3,.$ns) $::___zz___(cb4,.$ns) ;# and finally, we call the regular breakpoint if breakpoints not disabled
	set ncmd ""
#	puts "============= $vars ==============="
	foreach var $vars {
#		puts "var= |$var| "
		set cmd "array exist $var" ;# command to run in caller stack frame
		set arr [uplevel 1 $cmd]   ;# and now run it there
		if [catch {
			set cmd "if \{ ! \[ array exist $var \] \} \{ set $var \} else  \{ lsort \[ array names $var\]  \}     "
#			puts "cmd= |$cmd| "
			set aval [uplevel 1 $cmd]
			set ok 1
		} err_code] {
			puts "$err_code for $var, so skip it"
			set ok 0
		}
		if { ! $ok } {
			continue
		}
#		puts "var=$var  aval= |$aval| arr= |$arr|"
		set nsvar "::${ns}::${var}" ;# the variable our namespace, so we can push it back to the locals
		set comment_it_out ""
		if { $var eq "args" } {
#			set comment_it_out "#" ;# don't think it's a good idea to have the user change args, maybe we will change our minds
		}
		if { $arr } {
			append	ncmd "#array set $var \{() $nsvar\} ...\n" ;# here is where would could someday support local array variables
		} else {
#			append	ncmd "${comment_it_out}set $var \$$nsvar \n puts \"$var now is \$$var\" \n   " ;# debug in the proc's space
			append	ncmd "   ${comment_it_out}set $var \$$nsvar  \n"
		}
	}
#	puts "at bot ns= |$ns| ncmd= |\n$ncmd| "
	uplevel 1 $ncmd
#	namespace eval $ns $ncmd ;# set the variables in the user's local
#	$::___zz___(vw+) "${ns}::" .$ns


} 
proc instrument+ {procedure args} {
# this is a work in progress. it will add single stepping breakpoints
# to the code but there are cases where it won't work, like when there
# are switch statements on several lines. it appends a ;lbp+ step-instrument id
# where id is generated as a large number so it should be unique
# to use, do this:   eval [instrument+ procname]
# after the proc has been defined, this will re-define it with debug code

	set enable 1 ;# enable instrumenting
#	set pcode [getproc $procedure]
	set rbracket "\}"
	set norb 0
#	puts "args= |$args| "
	if { [lsearch "-norb" $args] >= 0 } {  
		set norb 1
#		puts "norb= |$norb| "
	}
	if { [lsearch "-revert" $args] >= 0 } {  
		if { [info exists  ::___zz___(original,$procedure)] } {
			eval $::___zz___(original,$procedure) ;# this automatically removes the trace, so we need not do it ourselves
			unset ::___zz___(original,$procedure) ;# remove this so it will fail if user tries it twice
#			set zzz [trace info execution $procedure]
#			puts "zzz= |$zzz| 1"
#			trace remove execution $procedure enter [list $::___zz___(util+) tracer]
#			set zzz [trace info execution $procedure]
#			puts "zzz= |$zzz| 2"
		} else {
			error "cannot find an original to revert to for $procedure"
		}
		return
	}
	if { [info exists ::___zz___(original,$procedure)] } {
		error "The proc $procedure is already instrumented, you must -revert first"
	}
	set pcode [apply { {proc} {
	
		set result ""
		set space ""
		set result "proc $proc \{"
		foreach arg [info args $proc] {
			if [info default $proc $arg value] {
				append result "$space{$arg $value}"
			} else {
				append result $space$arg
			}
			set space " "
		}
		append result "\} \{"
		append result [info body $proc]
		append result "\}\n"
		return $result
		
				
	}} $procedure] 
# lets stash away pcode, which has the original, so we can get it back
	set ::___zz___(original,$procedure) $pcode

#    puts "[string length $pcode]  [string length $pcode2] and [expr {$pcode eq $pcode2}]"
#	 puts "pcode = \n$pcode"

	set lines [split [string trimright $pcode \n] \n]
	set nlines [llength $lines]
	set ln 0
	set idn 1000000000
	set out ""
	foreach line $lines {
#		puts "line= |$line| "
		incr ln
		regsub {[ \t];#[ \t].*$} $line {} tline
		set tline [string trim $tline]
		set ok [info complete $tline] ;# not sure exactly how this works, but if its not complete, we may not want to instrument it
		# ok is now either 0 or 1, next we check for special cases and adjust ok based on that, but might just drop through
		set enable_seen 0
		if { [string index $tline 0] eq "#"} { ;# want to skip lines beginning with # so we don't step through comments
			
			if       { [string range $tline 0 7] eq  "#enable+" } {
				set enable 1
				set tline "" 
#				set line ""
				set enable_seen 1
			} elseif { [string range $tline 0 8] eq  "#disable+" } {
				set enable 0
			}
#			puts "enable= |$enable| tline= |$tline| "
#			set ok 3 
		} elseif {$tline eq ""} { ;# and don't want it for blank lines either 
#			set ok 4
		} elseif {[string index [string trim $tline] end] eq "\\"} { 
			# this actually shouldn't happen, parser removes them in info body, and so our line numbers
			# will also be correct if there's an error dialog, but we'll leave this code in anyway, wont hurt
			set ok 5
		} else {
			set  words [split $tline]
			set cmd [lindex $words 0]
			if { $cmd eq "return" || $cmd eq "lbp+"} {
				set ok 6
			}
		}
		if {  $ok == 0} { 
		    # if it's still 0, we have an incomplete statement, but not a comment, blank line, or continuation (see other comments on this)
			# however, for incomplete lines that are in the style I use, we'll add a breakpoint after the opening brace, 
			# at the end of a line, (like on this if statement) for the several sorts of statements that have these
			# also add one after the closing brace, if the only one on the line
			set  words [split $tline]
			set cmd [lindex $words 0]
			if { $cmd eq "switch" } {
				set norb 1 ;# if there's a switch statement seen, we apply the -norb option for here on
			}
#			puts "cmd= |$cmd| " ;# get the command word, first on the line, but could be a final closing brace on a line by itself
			set rb $rbracket
			if { $norb } {
				set rb "xxxxxxxxxxxxxxxxxxxxxxxx"
			}
			set keys [list if foreach proc for while  $rb ]
			if {$cmd  in $keys } {
#				puts "$cmd is in $keys"
				if { [string index $tline end] eq "\{"} {
					set ok 2 ;# some sort of control command ending in a open brace
				}
			}
		}
		incr idn 10000
		set id "bpid$idn"
		if       { $ok == 0 || $ok == 3 || $ok == 4 || $ok == 5 || $ok == 6} { ;# we only gave these differnt numbers for debugging use
			set instrument "" ;# if incomplete or one blank lines, comment only, a return, and line continuation (but shouldn't see that here) 
		} elseif { $ok == 2 || $ok == 1 } {
			
			set instrument ";lbp+ step-instrument $id" ;# here's what we append, note step-instrument is also looked for to strip these when listing code
		} elseif { 0 } {
			
		} else {
			set instrument "" ;# should not ever get here, but if we ever do, just don't instrument this line
		}
		if { $ln == $nlines } {
			set instrument ""
		}
#		puts "                                line [format %4d $ln]   ($ok) = $line $instrument"
#		puts "tline= |$tline| "
		if { $tline eq $rbracket && $norb} {
			set instrument ""
		} else {
#			set zzz [regsub  "([ \t]);##([ \t])" line {\1 $} ?varName?]
			set zzz [regsub -nocase -linestop -lineanchor {([ \t])(;#+)([ \t])(?!.*")} $line "\\1$instrument\\3\\2 " result]
#			puts "enable=$enable\nok= $ok\ntline = |$tline|\nline= |$line|\ninstrument= |$instrument|\nresult= |$result| \nzzz= |$zzz| \n"
#			puts "zzz= |$zzz| result= |$result| "
			if { $zzz == 1 && $enable} {
				set line $result
				set instrument ""
			}
			if { $instrument ne "" } {
				incr idn ;# if we are doing an instrument, incr in lower portion also, for debugging mostly
			}
		}
		if { $enable } {
#			puts "at the end enable= |$enable| "
			if { $enable_seen } {
				append out "$instrument ; $line \n"
			} else {
				append out "$line  $instrument\n"
			}
#			append out "$line $instrument\n"
		} else {
#			puts "NO the end enable= |$enable| "
			append out "$line\n"	
		}
	}
#	puts "procedure= |$procedure| "
	append out "trace add execution $procedure enter \{$::___zz___(util+) tracer\}\n"
	append out "trace add execution $procedure leave \{$::___zz___(util+) tracerend\}\n"
	return $out
}
