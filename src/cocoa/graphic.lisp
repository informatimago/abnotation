;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               graphic.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements graphic API on COCOA.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)

(defmethod set-color ((color ns:ns-color))
  [color set]
  [color setFill] 
  [color setStroke])


(defmethod set-color ((color symbol))
  (set-color (ecase color
               (:black      [NSColor blackColor])
               (:blue       [NSColor blueColor])
               (:brown      [NSColor brownColor])
               (:clear      [NSColor clearColor])
               (:cyan       [NSColor cyanColor])
               (:dark-Gray  [NSColor darkGrayColor])
               (:gray       [NSColor grayColor])
               (:green      [NSColor greenColor])
               (:light-Gray [NSColor lightGrayColor])
               (:magenta    [NSColor magentaColor])
               (:orange     [NSColor orangeColor])
               (:purple     [NSColor purpleColor])
               (:red        [NSColor redColor])
               (:white      [NSColor whiteColor])
               (:yellow     [NSColor yellowColor]))))

(defmethod set-color ((color vector))
  (set-color [NSColor colorWithCalibratedRed: (aref color 0)
                      green:(aref color 1)
                      blue:(aref color 2)
                      alpha: (if (< 3 (length color))
                               (aref color 3)
                               1.0)]))


(defmethod set-font (font-name size)
  [[NSFont fontWithName:(to-objc font-name) size: (coordinate size)] set])

(defmethod draw-string ((string string) (where point) &key attributes)
  (if attributes
    (let ((dictionary [NSMutableDictionary dictionary]))
      [dictionary setObject:[NSFont fontWithName:(to-objc (getf attributes :font "Maestro"))
                                    size:(coordinate (getf attributes :size 12.0))]
                  forKey:(to-objc "NSFont")]
      [(to-objc string) drawAtPoint:(to-objc where) withAttributes:dictionary])
    
    [(to-objc string) drawAtPoint:(to-objc where) withAttributes: [NSDictionary dictionary]]))


(defmethod draw-string ((string string) (where rect) &key (attributes [NSDictionary dictionary]))
  [(to-objc string) drawInRect:(to-objc where) withAttributes:attributes])


(defmethod stroke-path ((path cocoa-bezier-path))
  [(bezier-path path) stroke])

(defmethod fill-path ((path cocoa-bezier-path))
  [(bezier-path path) fill])

(defmethod set-stroke-width ((path cocoa-bezier-path) width)
  [(bezier-path path) setLineWidth:(coordinate width)])


(defmethod draw-clef ((clef keyword) (where point) size)
  (draw-string (string (ecase clef
                         ((:bass15mb :bass)     (code-char 63))
                         ((:treble15ma :treble) (code-char 38))))
               where :attributes (list :font "Maestro" :size size)))


;;;; THE END ;;;;
