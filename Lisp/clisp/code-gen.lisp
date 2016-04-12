
(in-package :cl-user)

(defpackage :code-gen
    (:use :common-lisp :common-utils)
    (:export :gen-perl-pkg
             :gen-java-class))

(in-package :code-gen)

(defun gen-perl-pkg (name attrs &key methods exports)
    (string-append
        (get-perl-header name)
        (get-perl-import attrs exports)
        (get-perl-constructor name attrs)
        (get-perl-funs methods :type :method)
        (get-perl-funs exports)
        (format nil "1;~%~%")))

(defun get-perl-header (name)
    (string-append
        (format nil "~%# $Id$~%~%")
        (format nil "use strict;~%~%")
        (format nil "package ~A;~%~%" name)
        (format nil "use Switch;~%")
        (format nil "use Carp;~%")
        (format nil "use PerlUtil qw( gen_getter gen_setter );~%~%")))

(defun get-perl-import (attrs exports)
    (string-append
        (format nil "sub import {~%")
        (format nil "~4Tmy $package = shift;~%")
        (format nil "~4T$package =~~ s/.*:://;~%~%")
        (format nil "~4T##export the constructor~%")
        (format nil "~4Tno strict 'refs';~%")
        (format nil "~4Tmy ($caller) = caller;~%")
        (format nil "~4T*{ $caller . '::' . $package } = \\&{$package};~%~%")
        (format nil "~4T##install getters and setters~%")
        (format nil "~{~4Tgen_getter( __PACKAGE__, '~A' );~%~}~%" attrs)
        (format nil "~{~4Tgen_setter( __PACKAGE__, 'set_~A' );~%~}~%" attrs)
        (format nil "~4T##other exports~%")
        (format nil "~{~4T*{ $caller . '::~A' } = \\&~:*~A;~%~}" exports)
        (format nil "}~%~%")))

(defun get-perl-constructor (name attrs)
    (string-append
        (format nil "sub ~A {~%" name)
        (format nil "~4Tmy ( ~{$~A~^, ~} ) = @_;~%~%" attrs)
        (format nil "~4Tbless sub {~%")
        (format nil "~8Tmy $op = shift;~%")
        (format nil "~8Tswitch ($op) {~%")
        (format nil "~{~12Tcase '~A'~24T{ return $~:*~A; }~%~}" attrs)
        (format nil "~12Telse~24T{ croak 'Invalid operation ' . $op; }~%")
        (format nil "~8T}~%")
        (format nil "~4T}, __PACKAGE__;~%")
        (format nil "}~%~%")
        (format nil "use overload q{\"\"} => sub {~%")
        (format nil "~8Tmy $self = shift;~%~%")
        (format nil "~8Tsprintf \"#<%s ~{~A: %s~^, ~}>\",~%" attrs)
        (format nil "~12T$package, ~{$self->~A()~^, ~};~%" attrs)
        (format nil "};~%~%")))

(defun get-perl-funs (funs &key (type :export))
    (string-join
        (mapcar
            (lambda (fun)
                (string-append
                    (format nil "sub ~A {~%" fun)
                    (when (eq type :method)
                        (format nil "    my $self = shift;~%"))
                    (format nil "~%}~%~%")))
            funs)))

(princ (gen-perl-pkg "Point" '("x" "y")
                    :methods '("move")
                    :exports '("distance")))

