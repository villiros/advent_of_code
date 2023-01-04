enum Resources = {Ore, Clay, Obsidian, Geode};

%enum ResourceOpt = R(Resources) ++ {Null};

array[Resources, Resources] of int: robot_costs;
% for the 1st example bluprint, this should be:
% robot_costs = [|4, 0, 0, 0, |2, 0, 0, 0, |3, 14, 0, 0 |2, 0, 7, 0|];

% 24 for part a, 32 for b
int: max_time;

% What should the factory make (if anything)
% at each minute (will be ready at the next one)
array[1..max_time] of var opt Resources: factory_instruction;
% Number of robots and resources at the end of each step
array[0..max_time, Resources] of var 0..max_time: num_robots;
array[0..max_time, Resources] of var int: num_resources;

% Our starting robot state
constraint num_robots[0, Ore] == 1;
constraint forall(r in Clay..) (num_robots[0, r] == 0);

% And resources
constraint forall(r in Resources) (num_resources[0, r] == 0);

% To tell a factory to make a robot, we need enough resources at the end of the previous step
constraint forall(t in 1..max_time) (
    (factory_instruction[t] != <>) ->
    forall(r in Resources) (    
      robot_costs[factory_instruction[t], r] <= num_resources[t-1, r]
    )
  );

% Robot production
constraint forall(t in 1..max_time, r in Resources)
  (num_robots[t,r] = num_robots[t-1,r] + bool2int(factory_instruction[t] == r));

% Resource production and consumption
constraint forall (t in 1..max_time, r in Resources) (
  let {var int: production = num_robots[t-1,r];
       var int: consumption =
          robot_costs[(factory_instruction[t]),r] default 0;} in
    num_resources[t,r] = num_resources[t-1,r] + production - consumption
);

solve maximize num_resources[max_time,Geode];

output [show(num_resources[max_time,Geode])];