;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful code for managing key presses

(in-package #:lispbuilder-sdl)

(defparameter *input-util-initialised* nil)
(defparameter *input-event-status* nil)

(defstruct input-status status time prev-time)

(defstruct joystick
  fp
  index
  axis-x-motion
  axis-y-motion
  button-down
  button-up
  hat-x-motion
  hat-y-motion
  ball-x-motion
  ball-y-motion
  status)

(defun input-eq (input input-status)
  "Returns true if the input is eq to input-status.

##### Returns
True if the input is held
"
  (let ((status (gethash input *input-event-status*)))
    (if (input-status-p status)
      (eq input-status (input-status-status status))
      nil)))

(defun input-set-p (input input-status)
  "Returns true if the status for input has just been set

##### Returns
True if the input was just pressed
"
  (when (and (input-eq input input-status)
             (= 0.0 (time-in-current-state input)))
    input-status))

(defun handle-input (input input-status)
  (let ((status (get-or-create-input-status input)))
    (setf (input-status-status status) input-status
          (input-status-prev-time status) (input-status-time status)
          (input-status-time status) 0.0)))

(defun time-in-current-state(input)
  "Returns time an input has been in current state

##### Returns
Time input is in current state
"
  (let ((status (gethash input *input-event-status*)))
    (if (input-status-p status)
	(input-status-time status)
	0.0)))

(defun time-in-previous-state(input)
  "Returns time input was in a previous state

##### Returns
Time input was in previous state
"
  (let ((status (gethash input *input-event-status*)))
    (if (input-status-p status)
	(input-status-prev-time status)
	0.0)))

(defun close-joystick (joystick)
  (when (> (sdl-cffi::sdl-joystick-opened (joystick-index joystick)) 0)
    (sdl-cffi::sdl-joystick-close (joystick-fp joystick))))

(defun get-or-create-input-status(input)
  "gets the status of a particular input device, for example key, mouse button or joystick.
If there is no existing status for the input type then a new status (unknown state)
is created and added to the hash table"
  (let ((status (gethash input *input-event-status*)))
    (if (input-status-p status)
      status
      (setf (gethash input *input-event-status*) 
            (make-input-status :status 'unknown :time 0.0 :prev-time 0.0)))))

(defun initialise-input-util()
  "Initialises the input util system. This just creates the data structure that input status information 
is stored in, and maintains a global variable to track initialisation status.

##### Parameters

##### Returns
"
  (unless *input-util-initialised*
    (setf *input-util-initialised* t
          *input-event-status* (make-hash-table))))

(defun debug-view-inputs()
  (loop for input being the hash-keys of *input-event-status* do
        (let ((status (gethash input *input-event-status*)))
          (format t "input ~a status ~a time ~a prev time ~a~%" input (input-status-status status) 
                  (input-status-time status) (input-status-prev-time status)))))

(defun update-input-util(time)
  "This must be called once for each time period you are updating the application, in order 
to update key, mouse button & joystick information. 

##### Parameters
* `TIME` is the time in seconds this update represents

##### Returns
"
;  (debug-view-keys)
  (loop for input-status being the hash-values of *input-event-status* do
       (incf (input-status-time input-status) time)))

(defun quit-input-util()
  "This is called when you quit your app to free up input information data

##### Parameters

##### Returns
"
  (setf *input-util-initialised* nil)
  (when *input-event-status*
    ;; Close the opened joysticks.
    (loop for input-status being the hash-values of *input-event-status* do
          (when (joystick-p (input-status-status input-status))
            (close-joystick (input-status-status input-status)))))
  (setf *input-event-status* nil))

;;;;; Begin key handling
;;;;; 

(defun handle-key-up(key)
  "You must call this when a key up event occurs

##### Parameters
* `KEY` is the SDL key definition for the key that is now up (for example :SDL-KEY-ESCAPE)
##### Returns
"
  (handle-input key 'released))

(defun handle-key-down(key)
  "You must call this when a key up event occurs

##### Parameters
* `KEY` is the SDL key definition for the key that is now down (for example :SDL-KEY-ESCAPE)
##### Returns
"
  (handle-input key 'pressed))

(defun key-held-p(key)
  "Returns true if a key is currently held, which means it has either
just been pressed, or it has been pressed and held for a while.

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key is held
"
  (input-eq key 'pressed))

(defun key-pressed-p(key)
  "Returns true if a key has just been pressed

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key was just pressed
"
  (input-set-p key 'pressed))

(defun key-released-p(key)
  "Returns true if a key has just been released

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
True if the key was just pressed
"
  (input-set-p key 'released))

(defun key-time-in-current-state(key)
  "Returns time a key has been in current state

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
Time key is in current state
"
  (time-in-current-state key))

(defun key-time-in-previous-state(key)
  "Returns time key was in a previous state

##### Parameters
* `KEY` is the SDL key definition (for example :SDL-KEY-ESCAPE)
##### Returns
Time key was in previous state
"
  (time-in-previous-state key))

;;;;;
;;;;; End key handling

;;;;; Begin mouse handling
;;;;;
(defun handle-mouse-up(button)
  "You must call this when a mouse button up event occurs

##### Parameters
* `BUTTON` is the SDL mouse button definition for the button that is now up. This can be one of
SDL-BUTTON-LEFT, SDL-BUTTON-MIDDLE, SDL-BUTTON-RIGHT, SDL-BUTTON-WHEEL-UP, SDL-BUTTON-WHEEL-DOWN,
SDL-BUTTON-X1, SDL-BUTTON-X2.
##### Returns
"
  (handle-input button 'released))

(defun handle-mouse-down(button)
  "You must call this when a mouse button down event occurs

##### Parameters
* `BUTTON` is the SDL mouse button definition for the button that is now down. This can be one of
SDL-BUTTON-LEFT, SDL-BUTTON-MIDDLE, SDL-BUTTON-RIGHT, SDL-BUTTON-WHEEL-UP, SDL-BUTTON-WHEEL-DOWN,
SDL-BUTTON-X1, SDL-BUTTON-X2.
##### Returns
"
  (handle-input button 'pressed))

(defun mouse-held-p(mouse)
  "Returns true if a mouse is currently held, which means it has either
just been pressed, or it has been pressed and held for a while.

##### Parameters
* `MOUSE` is the SDL mouse definition (for example :SDL-MOUSE-ESCAPE)
##### Returns
True if the mouse is held
"
  (input-eq mouse 'pressed))

(defun mouse-pressed-p(button)
  "Returns true if a mouse button has just been released

##### Parameters
* `MOUSE` is the SDL mouse definition (for example :SDL-MOUSE-ESCAPE)
##### Returns
True if the mouse was just pressed
"
  (input-set-p button 'pressed))

(defun mouse-released-p(button)
  "Returns true if a mouse has just been released

##### Parameters
* `MOUSE` is the SDL mouse definition (for example :SDL-MOUSE-ESCAPE)
##### Returns
True if the mouse was just pressed
"
  (input-set-p button 'released))

(defun mouse-time-in-current-state(button)
  "Returns time a mouse button has been in current state

##### Parameters
* `MOUSE` is the SDL mouse definition (for example :SDL-BUTTON-LEFT)
##### Returns
Time mouse is in current state
"
  (time-in-current-state button))

(defun mouse-time-in-previous-state(button)
  "Returns time mouse button was in a previous state

##### Parameters
* `MOUSE` is the SDL mouse definition (for example :SDL-BUTTON-LEFT)
##### Returns
Time mouse was in previous state
"
  (time-in-previous-state button))

;;;;; 
;;;;; End mouse handling

;;;;; Begin Joystick handling
;;;;;

;;(defun handle-joy-button-up(index button)
;;  "You must call this when a joystick up event occurs
;;
;;##### Parameters
;;* `BUTTON` is the definition for the joystick button that is now up.
;;##### Returns
;;"
;;  (handle-joystick-input index button 'released))

;;;;; 
;;;;; End Joystick handling
