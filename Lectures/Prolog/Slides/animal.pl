big(bear).            % Clause 1
big(elephant).        % Clause 2
small(cat).           % Clause 3
brown(bear).          % Clause 4
black(cat).           % Clause 5
gray(elephant).       % Clause 6
dark(Z) :- black(Z).  % Clause 7
dark(Z) :- brown(Z).  % Clause 8
