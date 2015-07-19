arm-iss
=======

arm-iss is in the planning/prototyping phase. Seriously, don't even
think about touching it yet...

Goals
-----

* Implement an instruction set simulator for ARM.
* Develop small, modular components and test them in isolation.
  * For example, the ALU only needs access to the register file and
    barrel shifter output to produce meaningful results. However, it
    doesn't need direct access to RAM.
* Favor fundamental features over exhaustive coverage and extensive
  validation.
  * Defer interrupts, exceptions, and mode switching until the basics
    work in USR mode.
  * Focus on ARM rather than Thumb state.
  * Keep the memory and bus architecture as simple as possible to
    demonstrate the chip's core behaves as expected.
* Aim for portability, supporting a variety of hardware architectures,
  operating systems, and Common Lisp implementations.


Running tests
-------------

Load the :arm-iss/test system using Quicklisp. The main system,
:arm-iss, is a dependency of the test system, so it comes along for
the ride.

    (ql:quickload "arm-iss/test")

Run the tests. The results are printed in the REPL.

    (uiop:symbol-call :5am :run! :arm-iss)

SLIME, ASDF, Quicklisp, or 5am itself may offer a better workflow, but
for now this is the only process I've tested. ASDF operations seem
promising, but require more investigation. See the TODO in
arm-iss.asd.
