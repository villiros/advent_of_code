with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p04 is
    type p04a is new SolutionDispatcher with record
        null;
    end record;
    type p04b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p04a;
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p04b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p04;
