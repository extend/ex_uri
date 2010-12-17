-record(ex_uri_authority, {userinfo :: string() | undefined,
                           host :: string(),
                           port :: integer() | undefined}).

-record(ex_uri, {scheme :: string(),
                 authority :: #ex_uri_authority{} | undefined,
                 path :: [string()],
                 q :: string() | undefined,
                 fragment :: string() | undefined}).

-record(ex_uri_ref, {authority :: #ex_uri_authority{} | undefined,
                     path :: [string()],
                     q :: string() | undefined,
                     fragment :: string() | undefined}).
