;; PARC's modular printer domain
;; compiled into PDDL by Rong Zhou
;; To report a problem, send email to rzhou@parc.com
;; Papers:
;; 1. Planning for Modular Printers: Beyond Productivity
;;	Minh B. Do, Wheeler Ruml, and Rong Zhou. ICAPS'08
;; 2. On-line Planning and Scheduling: An Application to Controlling Modular Printers
;;	Minh B. Do, Wheeler Ruml, and Rong Zhou. AAAI'08
;; 3. On-line Planning and Scheduling for High-speed Manufacturing
;;	Wheeler Ruml, Minh B. Do, and Markus P.J. Fromherz. ICAPS'05
(define (domain eTipp)
(:requirements :typing :action-costs)
(:types  size_t location_t side_t color_t image_t resource_t sheet_t)
(:constants
		Letter - size_t

		Black
		Color - color_t

		Front
		Back - side_t

		Some_Feeder_Tray
		Some_Finisher_Tray
		Some_Purge_Tray
		fe1_Exit-hw1_LeftEntry
		hw1_RightExit-sys_Entry
		uc1_Entry-hw1_TopLeftExit
		lc1_Entry-hw1_BottomLeftExit
		hw1_TopRightEntry-uc1_Exit
		lc1_Exit-hw1_BottomRightEntry
		uc1_ExitToIME-uime_Entry
		uime_Exit-uc1_EntryFromIME
		lime_Entry-lc1_ExitToIME
		lc1_EntryFromIME-lime_Exit
		sys_OutputTray - location_t

		fe1-RSRC
		hw1-RSRC
		uc1-RSRC
		uime-RSRC
		lc1-RSRC
		lime-RSRC
		sys-RSRC - resource_t
)
(:predicates
		(Sheetsize ?sheet - sheet_t ?size - size_t)
		(Location ?sheet - sheet_t ?location - location_t)
		(Hasimage ?sheet - sheet_t ?side - side_t ?image - image_t)
		(Sideup ?sheet - sheet_t ?side - side_t)
		(Stackedin ?sheet - sheet_t ?location - location_t)
		(Imagecolor ?image - image_t ?color - color_t)
		(Notprintedwith ?sheet - sheet_t ?side - side_t ?color - color_t)
		(Oppositeside ?side1 - side_t ?side2 - side_t)
		(Available ?resource - resource_t)
		(Prevsheet ?sheet1 - sheet_t ?sheet2 - sheet_t)
		(Uninitialized)

)
(:functions (total-cost) - number)
(:action initialize
 :parameters ()
 :precondition (and
		(Uninitialized))
 :effect (and
		(not (Uninitialized))
		(Available fe1-RSRC)
		(Available hw1-RSRC)
		(Available uc1-RSRC)
		(Available uime-RSRC)
		(Available lc1-RSRC)
		(Available lime-RSRC)
		(Available sys-RSRC))
)
(:action fe1-FeedMSI-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available fe1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet Some_Feeder_Tray))
 :effect (and
		(not (Available fe1-RSRC))
		(Location ?sheet fe1_Exit-hw1_LeftEntry)
		(Sideup ?sheet Back)
		(not (Location ?sheet Some_Feeder_Tray))
		(Available fe1-RSRC)
		(increase (total-cost) 125))
)
(:action fe1-Feed-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available fe1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet Some_Feeder_Tray))
 :effect (and
		(not (Available fe1-RSRC))
		(Location ?sheet fe1_Exit-hw1_LeftEntry)
		(Sideup ?sheet Back)
		(not (Location ?sheet Some_Feeder_Tray))
		(Available fe1-RSRC)
		(increase (total-cost) 224))
)
(:action hw1-LeftEntryToTopLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet fe1_Exit-hw1_LeftEntry))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet uc1_Entry-hw1_TopLeftExit)
		(not (Location ?sheet fe1_Exit-hw1_LeftEntry))
		(Available hw1-RSRC)
		(increase (total-cost) 1499))
)
(:action hw1-LeftEntryToBottomLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet fe1_Exit-hw1_LeftEntry))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet lc1_Entry-hw1_BottomLeftExit)
		(not (Location ?sheet fe1_Exit-hw1_LeftEntry))
		(Available hw1-RSRC)
		(increase (total-cost) 1499))
)
(:action hw1-TopRightEntryToTopLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet hw1_TopRightEntry-uc1_Exit))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet uc1_Entry-hw1_TopLeftExit)
		(not (Location ?sheet hw1_TopRightEntry-uc1_Exit))
		(Available hw1-RSRC)
		(increase (total-cost) 8999))
)
(:action hw1-TopRightEntryToBottomLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet hw1_TopRightEntry-uc1_Exit))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet lc1_Entry-hw1_BottomLeftExit)
		(not (Location ?sheet hw1_TopRightEntry-uc1_Exit))
		(Available hw1-RSRC)
		(increase (total-cost) 8999))
)
(:action hw1-BottomRightEntryToTopLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet lc1_Exit-hw1_BottomRightEntry))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet uc1_Entry-hw1_TopLeftExit)
		(not (Location ?sheet lc1_Exit-hw1_BottomRightEntry))
		(Available hw1-RSRC)
		(increase (total-cost) 8999))
)
(:action hw1-BottomRightEntryToBottomLeftExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet lc1_Exit-hw1_BottomRightEntry))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet lc1_Entry-hw1_BottomLeftExit)
		(not (Location ?sheet lc1_Exit-hw1_BottomRightEntry))
		(Available hw1-RSRC)
		(increase (total-cost) 8999))
)
(:action hw1-TopRightEntryToRightExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet hw1_TopRightEntry-uc1_Exit))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet hw1_RightExit-sys_Entry)
		(not (Location ?sheet hw1_TopRightEntry-uc1_Exit))
		(Available hw1-RSRC)
		(increase (total-cost) 1499))
)
(:action hw1-BottomRightEntryToRightExit-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available hw1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet lc1_Exit-hw1_BottomRightEntry))
 :effect (and
		(not (Available hw1-RSRC))
		(Location ?sheet hw1_RightExit-sys_Entry)
		(not (Location ?sheet lc1_Exit-hw1_BottomRightEntry))
		(Available hw1-RSRC)
		(increase (total-cost) 1499))
)
(:action uc1-ToIME-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available uc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet uc1_Entry-hw1_TopLeftExit))
 :effect (and
		(not (Available uc1-RSRC))
		(Location ?sheet uc1_ExitToIME-uime_Entry)
		(not (Location ?sheet uc1_Entry-hw1_TopLeftExit))
		(Available uc1-RSRC)
		(increase (total-cost) 2999))
)
(:action uc1-InvertToIME-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?otherface - side_t)
 :precondition (and
		(Available uc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Oppositeside ?face ?otherface)
		(Location ?sheet uc1_Entry-hw1_TopLeftExit)
		(Sideup ?sheet ?face))
 :effect (and
		(not (Available uc1-RSRC))
		(Location ?sheet uc1_ExitToIME-uime_Entry)
		(Sideup ?sheet ?otherface)
		(not (Location ?sheet uc1_Entry-hw1_TopLeftExit))
		(not (Sideup ?sheet ?face))
		(Available uc1-RSRC)
		(increase (total-cost) 8000))
)
(:action uc1-FromIME-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available uc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet uime_Exit-uc1_EntryFromIME))
 :effect (and
		(not (Available uc1-RSRC))
		(Location ?sheet hw1_TopRightEntry-uc1_Exit)
		(not (Location ?sheet uime_Exit-uc1_EntryFromIME))
		(Available uc1-RSRC)
		(increase (total-cost) 2999))
)
(:action uc1-InvertFromIME-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?otherface - side_t)
 :precondition (and
		(Available uc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Oppositeside ?face ?otherface)
		(Location ?sheet uime_Exit-uc1_EntryFromIME)
		(Sideup ?sheet ?face))
 :effect (and
		(not (Available uc1-RSRC))
		(Location ?sheet hw1_TopRightEntry-uc1_Exit)
		(Sideup ?sheet ?otherface)
		(not (Location ?sheet uime_Exit-uc1_EntryFromIME))
		(not (Sideup ?sheet ?face))
		(Available uc1-RSRC)
		(increase (total-cost) 8000))
)
(:action uime-Simplex-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?image - image_t)
 :precondition (and
		(Available uime-RSRC)
		(Sheetsize ?sheet Letter)
		(Sideup ?sheet ?face)
		(Imagecolor ?image Black)
		(Location ?sheet uc1_ExitToIME-uime_Entry)
		(Notprintedwith ?sheet ?face Black))
 :effect (and
		(not (Available uime-RSRC))
		(Location ?sheet uime_Exit-uc1_EntryFromIME)
		(Hasimage ?sheet ?face ?image)
		(not (Location ?sheet uc1_ExitToIME-uime_Entry))
		(not (Notprintedwith ?sheet ?face Black))
		(Available uime-RSRC)
		(increase (total-cost) 127790))
)
(:action lc1-ToIME-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available lc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet lc1_Entry-hw1_BottomLeftExit))
 :effect (and
		(not (Available lc1-RSRC))
		(Location ?sheet lime_Entry-lc1_ExitToIME)
		(not (Location ?sheet lc1_Entry-hw1_BottomLeftExit))
		(Available lc1-RSRC)
		(increase (total-cost) 4999))
)
(:action lc1-InvertToIME-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?otherface - side_t)
 :precondition (and
		(Available lc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Oppositeside ?face ?otherface)
		(Location ?sheet lc1_Entry-hw1_BottomLeftExit)
		(Sideup ?sheet ?face))
 :effect (and
		(not (Available lc1-RSRC))
		(Location ?sheet lime_Entry-lc1_ExitToIME)
		(Sideup ?sheet ?otherface)
		(not (Location ?sheet lc1_Entry-hw1_BottomLeftExit))
		(not (Sideup ?sheet ?face))
		(Available lc1-RSRC)
		(increase (total-cost) 9999))
)
(:action lc1-FromIME-Letter
 :parameters ( ?sheet - sheet_t)
 :precondition (and
		(Available lc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Location ?sheet lc1_EntryFromIME-lime_Exit))
 :effect (and
		(not (Available lc1-RSRC))
		(Location ?sheet lc1_Exit-hw1_BottomRightEntry)
		(not (Location ?sheet lc1_EntryFromIME-lime_Exit))
		(Available lc1-RSRC)
		(increase (total-cost) 4999))
)
(:action lc1-InvertFromIME-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?otherface - side_t)
 :precondition (and
		(Available lc1-RSRC)
		(Sheetsize ?sheet Letter)
		(Oppositeside ?face ?otherface)
		(Location ?sheet lc1_EntryFromIME-lime_Exit)
		(Sideup ?sheet ?face))
 :effect (and
		(not (Available lc1-RSRC))
		(Location ?sheet lc1_Exit-hw1_BottomRightEntry)
		(Sideup ?sheet ?otherface)
		(not (Location ?sheet lc1_EntryFromIME-lime_Exit))
		(not (Sideup ?sheet ?face))
		(Available lc1-RSRC)
		(increase (total-cost) 9999))
)
(:action lime-Simplex-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?image - image_t)
 :precondition (and
		(Available lime-RSRC)
		(Sheetsize ?sheet Letter)
		(Sideup ?sheet ?face)
		(Imagecolor ?image Color)
		(Location ?sheet lime_Entry-lc1_ExitToIME)
		(Notprintedwith ?sheet ?face Color))
 :effect (and
		(not (Available lime-RSRC))
		(Location ?sheet lc1_EntryFromIME-lime_Exit)
		(Hasimage ?sheet ?face ?image)
		(not (Location ?sheet lime_Entry-lc1_ExitToIME))
		(not (Notprintedwith ?sheet ?face Color))
		(Available lime-RSRC)
		(increase (total-cost) 212790))
)
(:action lime-SimplexMono-Letter
 :parameters ( ?sheet - sheet_t ?face - side_t ?image - image_t)
 :precondition (and
		(Available lime-RSRC)
		(Sheetsize ?sheet Letter)
		(Sideup ?sheet ?face)
		(Imagecolor ?image Black)
		(Location ?sheet lime_Entry-lc1_ExitToIME)
		(Notprintedwith ?sheet ?face Black))
 :effect (and
		(not (Available lime-RSRC))
		(Location ?sheet lc1_EntryFromIME-lime_Exit)
		(Hasimage ?sheet ?face ?image)
		(not (Location ?sheet lime_Entry-lc1_ExitToIME))
		(not (Notprintedwith ?sheet ?face Black))
		(Available lime-RSRC)
		(increase (total-cost) 212790))
)
(:action sys-Stack-Letter
 :parameters ( ?sheet - sheet_t ?prevsheet - sheet_t)
 :precondition (and
		(Available sys-RSRC)
		(Prevsheet ?sheet ?prevsheet)
		(Location ?prevsheet Some_Finisher_Tray)
		(Sheetsize ?sheet Letter)
		(Location ?sheet hw1_RightExit-sys_Entry))
 :effect (and
		(not (Available sys-RSRC))
		(Location ?sheet Some_Finisher_Tray)
		(Stackedin ?sheet sys_OutputTray)
		(not (Location ?sheet hw1_RightExit-sys_Entry))
		(Available sys-RSRC)
		(increase (total-cost) 1499))
)
)


