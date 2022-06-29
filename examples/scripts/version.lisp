#!/bin/sh

; SPDX-FileCopyrightText: 2017-2022 2017 John Mercouris, <john@atlas.engineer> et al.
;
; SPDX-License-Identifier: BSD-3-Clause

#|
exec nyxt --script "$0"
|#

(format t "This is a script running from Nyxt version ~a~&" +version+)
