(defpackage :python.syntax-example
  (:use :cl)
  (:export :run-calendar
	   :fetch-feed))

(in-package :python.syntax-example)

(python.syntax:enable-python-syntax)

(defun fetch-feed ()
  (burgled-batteries:startup-python)
  (burgled-batteries:import "feedparser")
  
  (unwind-protect
       (let (($url "http://pinterface.livejournal.com/data/atom"))
	 [^feedparser.parse($url)])
    (burgled-batteries:shutdown-python)))

(defun run-calendar ()
  (burgled-batteries:startup-python)
  (burgled-batteries:import "icalendar")
  (burgled-batteries:import "datetime")
  
  (unwind-protect
       (let (($cal [icalendar.Calendar()]))
	 [$cal.add('prodid', '-//My calendar product//mxm.dk//')]
	 (let (($event [icalendar.Event()]))
	   [$event.add('summary', 'Python meeting about calendaring')]
	   [$event.add('dtstart', datetime.datetime(2005,4,4,8,0,0))]
	   [$event.add('dtend', datetime.datetime(2005,4,4,10,0,0))]
	   [$event.add('dtstamp', datetime.datetime(2005,4,4,0,10,0))]
	   (let (($organizer [icalendar.vCalAddress('MAILTO: noone@example.com')]))
	     [$organizer.params['cn'] = icalendar.vText('Max Rasmussen')]
	     [$organizer.params['role'] = icalendar.vText('CHAIR')]
	     [$event['organizer'] = $organizer]
	     [$event['location'] = icalendar.vText('Odense, Denmark')]

	     [$event['uid'] = '20050115T101010/27346262376@mxm.dk']
	     [$event.add('priority', 5)]

	     (let (($attendee [icalendar.vCalAddress('MAILTO:maxm@example.com')]))
	       [$attendee.params['cn'] = icalendar.vText('Max Rasmussen')]
	       [$attendee.params['ROLE'] = icalendar.vText('REQ-PARTICIPANT')]
	       [$event.add('attendee', $attendee, encode=0)])

	     (let (($attendee [icalendar.vCalAddress('MAILTO:the-dude@example.com')]))
	       [$attendee.params['cn'] = icalendar.vText('The Dude')]
	       [$attendee.params['ROLE'] = icalendar.vText('REQ-PARTICIPANT')]
	       [$event.add('attendee', $attendee, encode=0)])

	     [$cal.add_component($event)]
	     [^$cal.to_ical()])))
    (burgled-batteries:shutdown-python)))


