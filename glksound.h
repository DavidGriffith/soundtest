#ifndef _GLKSOUND_H_;
Message "[Including <glksound>]";
System_file; Constant _GLKSOUND_H_;

!===============================================
! Prerequisites
! We need at least Inform 6.30 with Library 6/11
!===============================================
#ifdef VN_1630;
  Message "[glksound: ERROR - Unable to compile game]";
  Message "[glksound requires Inform 6.30 or later]";
  Message fatalerror "";
#endif; ! VN_1630
#ifndef LIBRARY_VERSION;
  Message "[glksound: ERROR - Unable to compile game]";
  Message "glksound requires Library 6/11 or later]";
  Message fatalerror "";
#endif; ! LIBRARY_VERSION
#iftrue (LIBRARY_VERSION < 611);
  Message "[glksound: ERROR - Unable to compile game]";
  Message "glksound requires Library 6/11 or later]";
  Message fatalerror "";
#endif; ! LIBRARY_VERSION

#ifdef TARGET_ZCODE;
  Message "[glksound: ERROR - Unable to compile game";
  Message "glksound works only for the Glulx machine";
  Message fatalerror "";
#endif; ! TARGET_ZCODE

!======================================
! Automatic entry points implementation
!======================================


! The programmer provides HandleGlkObject()
#ifdef IdentifyGlkObject;
    #ifndef GLKEXT_IGO;
	Message "[glksound: Using IdentifyGlkObject() as provided by programmer]";
	Message "  IMPORTANT: Make sure you call that in";
	Message "  GlkSound.IndentifySounds(phase)  **MANDATORY**";
    #endif;
#endif;

! The programmer provides HandleGlkEvent()
#ifndef HandleGlkEvent;
    #ifndef GLKSOUND_HGE;
	Message "[glksound: Using HandleGlkEvent() as provided by programmer]";
	Message "  IMPORTANT: Make sure you call that in";
	Message "  GlkSound.NotifyFade(ev)  **OPTIONAL**";
    #endif;
#endif;

!---------------------------------------------------------
! Initialization for unified extensions (Glk entry points)
Include "GlkExt";

#ifndef _GLKEXT_VERSION_;
    Message "[GlkSound: ERROR - Unable to compile program]";
    Message "  GlkExt version 3 or later is required";
    Message fatalerror "";
#endif;

#iftrue (_GLKEXT_VERSION_ < 3);
    Message "[GlkSound: ERROR - Unable to compile program]";
    Message "  GlkExt version 3 or later is required";
    Message fatalerror "";
#endif;

#ifdef GLKEXT_OK;
Object GlkSound_Glk GlkExt with
    IdentifyGlk [ phase;
	! Identify all sound objects present in GlkSound
	GlkSound.IdentifySound(phase);
    ],
    HandleGlk [ ev;
	! Report all fade effect (in, out) in "real-time" in GlkSound
	GlkSound.ReportFade(ev);
    ],
;
#endif;

! This is absolutely required for working with Glk
Include "infglk";


!===============================================================================
! I can't translate this...
!---
! We initialize extensions the way of Library 6/11.
! In this case, always call start game (routine just before 
! Initialise()) to GlkSound.InitGlk() to be created correctly the all-channel audio 
! and adequately prepare for use.
Object "(GlkSound_Ext)" LibraryExtensions with
    ext_initialise [;
	! Initialise GlkSound audio manager and channels
	! (normal / virtual / list)
	GlkSound.InitGlk();
    ],
;
!=============================================================================

Constant _GLKSOUND_VERSION_ = "3/150224";

#ifdef DEBUG;
    Constant ERROR_GLK_NO_FULL_AUDIO =
	"[NOTE: Your interpreter does not fully support audio!]^
	 [-Sound and music will not play in the background-]^";

    Constant ERROR_PLAYLIST_FULL =
	"[ERROR: The playlist of sounds is full!]^
	 [-Unable to add a sound because the list is full-]^";

    Constant ERROR_PLAYLIST_FADING =
	"[ERROR: Fade already in process!]^
	 [-Unable to fade the playlist-]^";

    Constant ERROR_FADE_DURING_FADE =
	"[ERROR: Fade already in process!]^
	 [-Unable to execute a new fade until the current one is finished-]";
#endif; ! DEBUG

!==============================================================================

! Useful constants
Constant GLKSOUND_ERROR_SND = -99;	! bad sound (unmanaged / wrong)
Constant GLKSOUND_ERROR_CNL = -99;	! bad channel (wrong number)
Constant GLKSOUND_GG_ROCK   = 510;	! rock channel zero (on these, add 1)
Constant GLKSOUND_VOLMAX    = $10000/100; ! max volume (percentage)
Constant GLKSOUND_NCHANMAX  = 10;	! max normal channels
Constant GLKSOUND_VCHANMAX  = 10;	! max virtual channels
Constant GLKSOUND_SNDLSTMAX = 10;	! max sounds available in playlist

! Array used only for GlkSound events
Array gg_glksound_event --> 4;		! You should always have 4 slots

!==============================================================================
Object GlkSound
    ! Private routines and variables related to the GlkSound nucleus
    private
	glk_no_audio	false,	! Interpreter can't play samples or MODs
	audio_active	true,	! Audio output of GlkSound (default always on)
	num_vchan	0,	! Number of virtual channels used
	type_fade	0,	! What kind of fade
	vol_fade	0 0 0,	! Vol fade data -> 0 == vol.init;
						-> 1 == vol.final;
						-> 2 == vol.orig;
	chan_fade	-1,	! Normal channel assigned to the sound faded
				! [-1 'no channel']
	tick_fade	0,	! Save computation time for each tick of the
				! current Fade (tricks UNDO/RESTORE/RESTART)
	snd_pfadeout	0 0,	! Sound must touch the Fade -> 0 == sound;
							    -> 1 == Notify;
	vol_global	100,	! Global vol percent [0..100] default 100
	vol_vchan	100,	! Comm virt channel vol % [0..100] default 100
	vol_lchan	100,	! Comm playlist vol % [0..100] default 100
	gg_lchan	0,	! Internal reference channel (playlist, sound)
	gg_nchan 0 0 0 0 0 0 0 0 0 0, ! Internal references of 'normal channels'
	gg_vchan 0 0 0 0 0 0 0 0 0 0, ! Internal references of 'virtal channels'
	snd_chan 0 0 0 0 0 0 0 0 0 0, ! Sound played on every normal channel
	vol_chan 0 0 0 0 0 0 0 0 0 0, ! Volume of each normal channel (percent)
	reps_chan 0 0 0 0 0 0 0 0 0 0,! Repetitions of each normal channel
				      !   ('LOOP' indicated by -1)
	stat_chan 0 0 0 0 0 0 0 0 0 0,! Sound status of each normal channel
				      !   0==det./rep.finite;
				      !   1==playback;
	pib_reg 0 0 0 0 0 0 0 0 0 0 0,! state registration PIB
				      !   (UNDO/RESTORE/RESTART)
				      !   -> 10 == audio_activated
	snd_lst 0 0 0 0 0 0 0 0 0 0   ! Array of 'Playlist.Sound'
		0 0 0 0 0 0 0 0 0 0,  ! (slots GlkSound_SNDLSTMAX == * 2)
				      ! guard column par sounds;
				      ! Odd, waiting times

	! [KERNEL-1] UPDATE EVERYTHING RELATED TO NORMAL SPECIFIC CHANNEL
	! (playback state, volume, no sound, audio disabled, etc.)
	UpdateChannel [ channum		! number of channels,
	    esn			! ESN: arg. dark, SoundNotify? 0no 1yes
	    pib			! arg. dark, update state PIB?
				! UpdateChannel used () from
				! IdentifySound two ()
	    flag_ok_play;	! Copy the channel?
				! When the state PIB indicate
				! that no, will be false
	    !-------------------------------------------------------
	    ! If audio is not supported, do nothing more
	    if (self.glk_no_audio) { return; }
	    !-------------------------------------------------------
	    ! Stop unless assigned sound channel or state of sound is
	    ! zero (stopped or 'released with finite play')
	    if ((self.&snd_chan-->nc == 0) || (self.&stat_chan-->nc == 0)) {
	        ! Stop current channel and return doing nothing more
		glk_schannel_stop(self.&gg_nchan-->nc);
		self.InitPIB(nc, pib); ! VERY IMPORTANT : UPDATED STATUS PIB
		return;
	    }
	    !-------------------------------------------------------
	    ! only if that sound should not play indefinitely [rep ~= -1]
	    ! put the 'state of the sound' == 0 ('release with finite play')
	    if (self.&reps_chan-->nc ~= -1) { self.&stat_cnl-->nc = 0; }
	    !-------------------------------------------------------
	    ! VERY IMPORTANT: STATE REGISTER NOW UPDATED PIB AND BE AWARE IF
	    flag_ok_play = self.InitPIB(nc, pib); ! YOU MUST 'RE-LAUNCH' CHANNEL
	    !-------------------------------------------------------
	    ! IF THIS AUDIO OUTPUT IS OFF: STOP CHANNEL AND DON'T PLAY
	    ! *** NOTE: DO NOT CHANGE THE POSITION OF THIS CODE!!! ***
	    if (~~self.audio_active) {
		! stop the current channel, and return and do nothing more
		glk_schannel_stop(self.&gg_nchan-->nc);
		return;
	    }
	    !-------------------------------------------------------
	    ! IF ALL OF THE ABOVE HAS BEEN PASSED, THE PLAY BEGIN NOW
	    !-------------------------------------------------------
	    ! Can the terp change the volume?  Yes? Change it now.
	    if (glk_gestalt(gestalt_SoundVolume,0)) {
		glk_schannel_set_volume(self.&gg_nchan-->nc, self.&vol_chan-->nc * GLKSOUND_VOLMAX);
	    }
	    ! and now we actually play the sound...
	    if (flag_ok_play) { ! provided that the upgrade PIB is not negated!
		glk_schannel_play_ext(
		    self.&gg_nchan-->nc,	! channel
		    self.&snd_chan-->nc,	! sound
		    self.&rep_chan-->nc,	! repetitions
		    esn); ! generate event SoundNotify at the end?
			  ! (0 == no; 1 == yes)
	    }
	],

	! [KERNEL-2] MAKE A MASSIVE UPDATE OF ALL NORMAL CHANNELS
	! Besides the upgrade status register PIB was invoked in the case
	! is called with 'pib' == 1 [exclusive in IdentifySound()]
	UpdateChannels [ pib ! arg. dark, update the status register PIB?
	    i;	! for iterations
	    !-------------------------------------------------------
	    ! NOTE: no check here if Glk supports sound or output
	    ! audio is enabled, because it does own routine down
	    !-------------------------------------------------------
	    ! go through all the normal channels and update them
	    ! (with your registration PIB
	    !-------------------------------------------------------
	    for (i=0 : i<GLKSOUND_NCHANMAX : i=i+1) { self.UpdateChannels(i,0,pib); }
	    !-------------------------------------------------------
	    ! VERY IMPORTANT: Save current state of audio activity.

	    #ifndef GLKSOUND_DONT_PROTECT_SOUND;
	    if (pib == 1) { self.&pib_reg-->GLKSOUND_NCHANMAX = self.audio_active; }
	    #endif;
	],

	! [KERNEL-3] RECORD UPDATE STATE NORMAL GDP OF A SPECIFIC CHANNEL
	! [used exclusively in the internal routine Kernel-1 UpdateChannel()]

 	! The record of the state of PIB of normal channels GlkSound is 
	! a complex trick to bring you sounds that are not replayed if 
	! currently 'playing in the background' (ie, repeating 
	! themselves with 'rep. infinite' [stat_chan == 1]) and the 
	! player uses UNDO/RESTORE/RESTART in the game.
	! Glk does not have a system to preserve the "state of playback" 
	! of sounds, so when references are found [in 
	! IdentifyGlkObject()] it is necessary to "re-launch" the 
	! playback of all sounds that are needed.
	! The "State Register PIB" is an array that is "synchronized" 
	! with the channel data (specifically, the channel sounds and 
	! with its "state of playback", if present this "playing in the 
	! background").  Thanks to a call in Glulx assembly, you can 
	! "protect" PIB registry data from the UNDO/RESTORE/RESTART 
	! command so as to "compare" the data retrieved from the 
	! GlkSound kernel then to change the "state of play".  This 
	! "comparison" achieves the "magic because if sounds are 
	! currently "playing in the background" (according to the 
	! GlkSound kernel) and if those same sounds "were already 
	! playing in the background" before the UNDO/RESTORE/RESTART is 
	! done, the sound is still "playing" and should not be 
	! re-launched.  And it is used to update the PIB.
	! [The call to protect the PIB record is in IdentifySounds()]
	! [If UpdatePIB() returns TRUE, UpdateChannel should not be re-released]
	! ['PIB' means: "Playing In Background" =P]
	UpdatePIB [ channum pib ! pib: calculate channel re-launch
	    ok_play; !  flag indicating whether to "re-launch" the channel
	    !=============================================================
	    ! ** THE FOLLOWING IS JUST A WARNING TO AVOID WHEN COMPILING
	    ! ** WITHOUT USING THE "PROTECTION OF SOUNDS" PROVIDED BY GLKSOUND
	    !=============================================================
		#ifndef GLKSOUND_DONT_PROTECT_SOUNDS;
		nc = nc; pib = pib;	! prevent unused variable warning
		#endif;
	    !=============================================================
	    ok_play = true; ! Ideally, you shoudl always re-launch the channel
	    #ifndef GLKSOUND_DONT_PROTECT_SOUNDS;
	    !------------------------------------------------------------
	    ! PART 1: CHECK IF RE-LAUNCHING CHANNEL IS OKAY (AS PIB)
	    !------------------------------------------------------------
	    ! 'pib' == 1 only when called from IdentifySounds()
	    !------------------------------------------------------------
	    if (pib == 1) { ! '1' INDICATES THAT YOU SHOULD CONSIDER RE-LAUNCH
		! if the sound currently assigned to the channel is the
		! same as the sound "prior-to-change-of-state-of-game"
		! (recorded in PIB)...
		if (self.&snd_chan-->nc == self.&pib_reg-->nc) {
		    ! and if PIB indicates that before the 
		    ! "change-of-state" audio IF I WAS ON (because we 
		    ! can "recover" with audio disabled, in which case 
		    ! we don't "re-launch" channels)
		    if (self.&pib_reg-->GLKSOUND_NCHANMAX == true) { ! DON'T CLEAR!
			ok_play = false; ! Indication that the channel must
					 ! be re-launched.
		    }
		}
	    } else {
		! ** PIB ~= 1 when called outside IdentifySounds() **
		! [Now we save the activation state of audio if PIB == 1]
		! [active is saved from UpdateChannels()]
		self.&pib_reg-->GLKSOUND_NCHANMAX = self.audio_active; ! DON'T CLEAR!
	    }
	    !------------------------------------------------------------
	    ! PART 2: STATE REGISTER IS NOW PIB CHANNEL INDICATED
	    !------------------------------------------------------------
	    if (self.&stat_chan-->nc == 1) { ! if current channel "playing in background"
		self.&pib_reg-->nc = self.&snd_chan-->nc;	! keep that data in PIB
	    } else {
		self.&pib_reg-->nc = 0;      ! null in PIB (VERY IMPORTANT!)
		!------------------------------------------------------------
		! The only thing this code does is to "remember" only those
		! channels that	are 'playing in background' and "calculating",
		! consequently, their "potential re-launches".  If a channel
		! is not 'playing in background', there is no reason to be
		! "remembered" by the PIB registry. :P
		!------------------------------------------------------------
	    }
	    #endif; ! GLKSOUND_DONT_PROTECT_SOUNDS
	    !------------------------------------------------------------
	    ! VERY IMPORTANT: Finally return an indication of whether
	    ! or not it is allowed
	    ! "re-launch" channel [UpdateChannel() uses this value
	    return ok_play;
	],

	! Pauses using the timer [used by PlayList() and Fade ***X()]
	Pausing [ ms;	! miliseconds during the last timed pause
	    glk_request_timer_events(ms);
	    for (::) { ! an infinite loop
		glk_select(gg_glksound_event); ! find the generated event
		switch (gg_glksound_event-->0) {
		    !--------------------------------------------------------
		    ! If the timer generated a tick, exit the loop.
		    evtype_Timer:
			jump GLKSOUND_PAUSE_END;
		    !--------------------------------------------------------
		    ! Was there no change?  Then we must redraw the status
		    ! bar and update windows
		    evtype_Arrange, evtype_Redraw:
			DrawStatusLine();
			HandleGlkEvent(gg_glksound_event,1,gg_arguments);
		}
	    }
	    .GLKSOUND_PAUSE_END; ! auxiliary tag to exit the loop
	    ! finally stop the 'ticks' timer
	    glk_request_timer_events(0);
	],

	! This routine is automatically called after making an
	! UNDO/RESTORE/RESTART [this is called from IdentifySounds()]
	! and simply serves to check if there is an active Fade in the
	! "new state of play".  If so, restart the Timer
	! (could be disabled) to continue the Fade
	RecoverFade [;
	    ! Can the interpreter handle Time?  If not, do nothing more.
	    if (glk_gestalt(gestalt_Timer,0) == 0) { return; }
	    !------------------------------------------------------------
	    ! Is a Fade in progress?  If not, do nothing more.
	    if (~~self.InFade()) { return; }
	    !------------------------------------------------------------
	    ! If this channel is fading 'sound in background'...
	    if (self.&stat_chan-->(self.chan_fade) == 1) {
		glk_request_timer_events(self.tick_fade); ! restart the Timer
	    } else { ! If this channel is NOT fading 'sound in background'...
		self.AbortFade();	! ...end the Fade effect completely!!
	    }
	],

	! Fade effect return (1 == FadeIn or 2 == FadeOut)
	! [called by ReportFade()]
	DoFade [;
	    ! What kind of Fade?
	    ! (according to type: percentage up / down)
	    switch (self.type_fade) {
		1:  ! In FadeIn
		    ! Up one unit of volume
		    self.&vol_fade-->0 = (self.&vol_fade-->0)+1;
		    !--------------------------------------------------
		    ! While 'initial volume' <= 'final volume'
		    if ((self.&vol_fade-->0) <= (self.&vol_fade-->1)) {
			! % change in volume in this 'tick' Timer
			self.VolumeChannel(self.chan_fade, self.&vol_fade-->1)) {
			!----------------------------------------------
			! If we reach 'final volume', time to finish the fade
			if ((self.&vol_fade-->0) >= (self.&vol_fade-->1)) {
			    self.AbortFade();	! Abort the Fade
			    return;		! and end here.
			}
		    }

		2:  ! In FadeOut
		    ! Down one unit of volume
		    self.&vol_fade-->0 = (self.&vol_fade-->0)-1;
		    !--------------------------------------------------
		    ! while 'initial volume' is >= 'final volume'
		    if ((self.&vol_fade-->0) >= (self.&vol_fade-->1)) {
			! % change in volume in this 'tick' Timer
			self.VolumeChannel(self.chan_fade, self.&vol_fade-->0);
			!----------------------------------------------
			! If we reach 'final volume', time to finish the fade
			if ((self.&vol_fade-->0) <= (self.&vol_fade-->1)) {
			    self.AbortFade();	! Abort the Fade
			    !------------------------------------------
			    ! Also, if we have a sound post-fadeout, we 
			    ! play the played sound and if we have to 
			    ! report events we indicate...
			    self.Play(self.&snd_pfadeout-->0, self.&snd_pfadeoute-->1);
			}
			return;		! and end here
		    }
		}
	    }
	],

	! Check if the interpreter has full audio support.  If so, 
	! set the flag 'glk_no_audio' to report back to the Damusix kernel.
	GlkAudio [
	    aux;	! Display textual notice? (1 == yes, 0 == no)
	    ! Check if you have full audio support and notify kernel
	    if (self.TestAudio()) { self.glk_no_audio = false; } ! full support
	    else { self.glk_no_audio = true; } ! not full support
	    !------------------------------------------------------------
	    #ifdef DEBUG;
		! If audio is not supported, complain
		if ((aux==1) && (self.glk_no_audio)) {
		    glk_set_style(style_Preformatted);
		    new_line;
		    print (string) ERROR_GLK_NO_FULL_AUDIO;
		    new_line;
		    glk_set_style(style_Normal);
		}
	    #endif;
	    !------------------------------------------------------------
	    aux = aux;	! avoid compiler warning
	],

    !====================================================================
    !--------------------------------------------------------------------
    ! *** INITIALIZE DAMUSIX (PROGRAMMER NEVER CALLS THIS ROUTINE) ***
    !--------------------------------------------------------------------
    !====================================================================
    with
	! [AUX KERNEL] INITIALISE DATAMUSIX'S AUDIO MANAGER AND ALL CHANNELS
	! (NORMAL/VIRTUAL) * NOTE: PROGRAMMER NEVER CALLS THIS ROUTINE
	InitialiseGlk [
	    i;	! for iterations
	    !------------------------------------------------------------
