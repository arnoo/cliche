;
;   Copyright 2014 Arnaud Betremieux <arno@arnoo.net>
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.
;

(in-package :cl-user)

(defpackage :cliche-asd
  (:use :cl :asdf))

(in-package :cliche-asd)

(defsystem :cliche
  :name "cliche"
  :maintainer "Arnaud Betremieux"
  :licence "GPL"
  :serial t
  :version "0.1"
  :components ((:file "cliche"))
  :depends-on (
               :anaphora
               :cl-gd
               :cl-json
               :cl-markup
               :clutch
               :parenscript
               :puri
               :hunchentoot
               :tagger
               :zpb-exif
              )
  )
