with p00;
with p01;
with p02;
with p03;
with p04;
with p05;
with p06;
with p07;
with p08;
with p09;
-- ADVENT_ADD_WITH

package body solutions is
    function GetSolution(Name : ProblemName) return SolutionDispatcherAcc is
        InvalidSolutionName : exception;
        
        D : Dispatchers;
    begin
        p00.GetDispatchers(D);
        p01.GetDispatchers(D);
        p02.GetDispatchers(D);
        p03.GetDispatchers(D);
        p04.GetDispatchers(D);
        p05.GetDispatchers(D);
        p06.GetDispatchers(D);
        p07.GetDispatchers(D);
        p08.GetDispatchers(D);
        p09.GetDispatchers(D);
        -- ADVENT_ADD_GET_DISPATCHERS

        for I of D loop
            if I.Name = Name then
                return I;
            end if;
        end loop;

        raise InvalidSolutionName with "Invalid solution name";
    end GetSolution;

end solutions;
