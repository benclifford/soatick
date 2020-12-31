# soatick

soatick - generate a serial number suitable for use in a DNS SOA
record, based on the current date, but keeping a state file so that
if run multiple times on the same day, then a new serial number will
be generated

serial number format:

2011010502
YYYYMMDDNN

NN starts at 00 and increments by one each time soatick is called on the
same day. If NN overflows, carrys will propagate treating the serial
number as an integer (which may cause the DD and MM fields to move outside
of the range expected for months and days).
Calls to soatick on subsequent days will output values so that the serial
number still increases - i.e. behaviour will still be safe.

Algorithm:
  A = state file, incremented by 1. If the state file does not exist,
      treat it as containing 0.
  B = today's base serial number, NN=00
  Output = max(A,B)
  Output goes to state file and to stdout