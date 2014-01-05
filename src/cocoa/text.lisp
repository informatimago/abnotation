;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               text.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Text stuff.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-29 <PJB> Created.
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
(in-package :abnotation.cocoa)

(defun ns-data-with-ns-string (string)
  [NSData dataWithBytes:[string cStringUsingEncoding:#$NSUTF8StringEncoding]
          length:[string lengthOfBytesUsingEncoding:#$NSUTF8StringEncoding]])


(defun box-for-rtf-text (text)
  (get-nsrect [[[NSAttributedString alloc] initWithRTF:(ns-data-with-ns-string (to-objc text))
                documentAttributes:oclo:*null*] 
               boundingRectWithSize: (ns:make-ns-size 10000.0 10000.0)
               options:0]))


(defmethod compute-box-size ((element text) partition)
  (declare (ignore partition))
  (let ((r (box-for-rtf-text (rtf element))))
    (setf (slot-value element 'box) (dot/inch->dot/mm r))))


;;;; THE END ;;;;
