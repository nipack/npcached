%% Copyright
-author("nipack").

-record( values, {key,
                  value,
                  flags,
                  expire,        %% expire time second.
                  created_at,    %% create time.
                  updated_at} ). %% update time.