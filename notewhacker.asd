;;;; Copyright 2013 Janne Nykopp

;;;; notewhacker.asd

;;;;    This file is part of Notewhacker.
;;;;
;;;;    Notewhacker is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    Notewhacker is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with Notewhacker.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem #:notewhacker
  :serial t
  :description "Displays scrolling notes on screen, which the user should play with a midi-enabled instrument."
  :author "Janne Nykopp <newcup@iki.fi>"
  :license "GPLv3"
  :version "0.0.3"
  :depends-on (#:bordeaux-threads
               #:cl-opengl
               #:zpb-ttf
               #:vecto
               #:sdl2
               #:fiveam
               #:local-time
               #:cffi)
  :components ((:file "package")
               (:file "graphics")
               (:file "midi")
               (:file "oss-midi")
               (:file "jack-cffi")
               (:file "jack-midi")
               (:file "notation")
               (:file "matcher")
               (:file "notewhacker")))

