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

    (push #p"./" asdf:*central-registry*)
    (ql:quickload "arm-iss/test")
    (arm-iss-test:run-tests)
