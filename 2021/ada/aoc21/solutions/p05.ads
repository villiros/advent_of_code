with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p05 is
    type p05a is new SolutionDispatcher with record
        null;
    end record;
    type p05b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p05a;
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p05b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p05;
