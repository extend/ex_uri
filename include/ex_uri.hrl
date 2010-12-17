-record(ex_uri_authority, {userinfo :: string(),
                           host :: string(),
                           port :: integer()}).

-record(ex_uri, {scheme :: string(),
                 authority :: #ex_uri_authority{},
                 path :: [string()],
                 q :: string(),
                 fragment :: string()}).

-record(ex_uri_ref, {authority :: #ex_uri_authority{},
                     path :: [string()],
                     q :: string(),
                     fragment :: string()}).
