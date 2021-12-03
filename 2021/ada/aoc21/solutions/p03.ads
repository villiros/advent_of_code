with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p03 is
    type p03a is new SolutionDispatcher with record
        null;
    end record;
    type p03b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p03a;
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p03b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p03;
