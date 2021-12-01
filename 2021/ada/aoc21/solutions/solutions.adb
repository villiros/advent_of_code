with p01a_pkg;
with p01b_pkg;

package body solutions is
    function GetSolution(Name : ProblemName) return SolutionAcc is
        InvalidSolutionName : exception;
    begin
        if Name = "p01a" then
            return p01a_pkg.Make;
        elsif Name = "p01b" then
            return p01b_pkg.Make;
        --elsif Name = "p02a" then
        --    return p02a_pkg.Make;
        else
            raise InvalidSolutionName with "Invalid solution name";
        end if;
    end GetSolution;

end solutions;
