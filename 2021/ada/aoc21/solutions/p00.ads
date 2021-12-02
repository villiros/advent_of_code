with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p00 is
    type p00a is new SolutionDispatcher with record
        null;
    end record;
    type p00b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p00a;
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p00b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p00;