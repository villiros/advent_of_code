with p01a_pkg;
with p01b_pkg;

package body solutions is
    function GetSolution(Name : ProblemName) return SolutionAcc is
        InvalidSolutionName : exception;
    begin
        if Name = "p01a" then
            return new p01a_pkg.p01a;
        elsif Name = "p01b" then
            return new p01b_pkg.p01b;
        else
            raise InvalidSolutionName with "Invalid solution name";
        end if;
    end GetSolution;

end solutions;