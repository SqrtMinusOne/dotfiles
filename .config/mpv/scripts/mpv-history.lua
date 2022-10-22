local utils = require 'mp.utils'
local config_folder = '/logs-sync/mpv';
local log = os.getenv('HOME')..config_folder..'/'..os.date('%Y-%m-%d')..'.log';

local path;

mp.register_event('file-loaded', function ()
      local logfile = io.open(log, 'a+');
      path = mp.get_property('path');
      local data = {
         ['kind'] = 'loaded',
         ['time'] = os.date('!%Y-%m-%dT%TZ'),
         ['path'] = path,
         ['filename'] = mp.get_property('filename'),
         ['length'] = mp.get_property('duration'),
      };
      logfile:write(utils.format_json(data)..'\n');
      logfile:close();
end)

mp.observe_property('pause', 'bool', function (name, value)
      if (not path) then
         return;
      end
      local data = {
         ['kind'] = value and 'pause' or 'play',
         ['time'] = os.date('!%Y-%m-%dT%TZ'),
         ['path'] = path,
         ['pos'] = mp.get_property('time-pos'),
      }
      local logfile = io.open(log, 'a+');
      logfile:write(utils.format_json(data)..'\n');
      logfile:close();
end)

mp.register_event('seek', function ()
      if (mp.get_property_bool('pause')) then
         return;
      end
      local data = {
         ['kind'] = 'seek',
         ['time'] = os.date('!%Y-%m-%dT%TZ'),
         ['path'] = path,
         ['pos'] = mp.get_property('time-pos'),
      }
      local logfile = io.open(log, 'a+');
      logfile:write(utils.format_json(data)..'\n');
      logfile:close();
end)

mp.register_event('end-file', function (data)
      local kind;
      if (data['reason'] == 'eof') then
         kind = 'end';
      elseif (data['reason'] == 'quit' or data['reason'] == 'stop') then
         kind = 'stop';
      end;
      local data = {
         ['kind'] = kind,
         ['time'] = os.date('!%Y-%m-%dT%TZ'),
         ['path'] = path,
         ['pos'] = mp.get_property('time-pos'),
      }
      local logfile = io.open(log, 'a+');
      logfile:write(utils.format_json(data)..'\n');
      logfile:close();
end)
