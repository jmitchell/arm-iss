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
