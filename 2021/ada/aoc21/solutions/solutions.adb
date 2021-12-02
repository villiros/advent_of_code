with p00;
with p01;
-- ADVENT_ADD_WITH

package body solutions is
    function GetSolution(Name : ProblemName) return SolutionDispatcherAcc is
        InvalidSolutionName : exception;
        
        D : Dispatchers;
    begin
        p00.GetDispatchers(D);
        p01.GetDispatchers(D);
        -- ADVENT_ADD_GET_DISPATCHERS

        for I of D loop
            if I.Name = Name then
                return I;
            end if;
        end loop;

        raise InvalidSolutionName with "Invalid solution name";
    end GetSolution;

end solutions;
