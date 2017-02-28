-define(IS_RECEIVER(X), (is_pid(X) orelse is_atom(X))).
-define(IS_WORKER(X), (is_function(X) orelse 
                       (is_tuple(X) andalso (size(X) == 3 orelse size(X) == 2)) orelse 
                       is_atom(X))).

