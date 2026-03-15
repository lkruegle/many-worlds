-- | A Declarative EDSL for text-based adventure games
--
-- The ManyWorlds package implements an EDSL for declaring text-based adventure
-- games as well as a simple engine for validating and running your games.
--
-- You can import this module to access the entire public API, import any of the
-- sub modules to access the individual API components.
--
-- * ManyWorlds.WorldBuilder: Houses the EDSL, everything you need to build your
--      worlds.
--
-- * ManyWorlds.WorldRunner: Exports the functions for running built worlds.
--      This module provides some simple default behavior that can be modified
--      using a config.
--
-- * ManyWorlds.WorldConfig: Provides an API for configuring the world runner.
--      If you want to change the default behavior of WorldRunner, look here.
module ManyWorlds
  ( module ManyWorlds.WorldBuilder,
    module ManyWorlds.WorldRunner,
    module ManyWorlds.WorldConfig,
    module ManyWorlds.WorldSolver,
  )
where

import ManyWorlds.WorldBuilder
import ManyWorlds.WorldConfig
import ManyWorlds.WorldRunner
import ManyWorlds.WorldSolver
