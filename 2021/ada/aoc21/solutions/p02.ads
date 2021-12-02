with Ada.Text_IO;
with Ada.Containers.Vectors;

with Harness; use Harness;

package p02 is
    type p02a is new SolutionDispatcher with record
        null;
    end record;
    type p02b is new SolutionDispatcher with record
        null;
    end record;

    procedure GetDispatchers(D : in out Dispatchers);

    overriding
    function Solve (SDisp : p02a;
                    InData : in Ada.Text_IO.File_type) return ResultType;
    overriding
    function Solve (SDisp : p02b;
                    InData : in Ada.Text_IO.File_type) return ResultType;

end p02;
