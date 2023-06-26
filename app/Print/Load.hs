{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Print.Load (print) where

import Control.Concurrent (MVar)
import Lemonbar qualified as L
import State.Load as Load
import Utils.Types (Index)
import Prelude

-- void Load::print_raw(Lemonbar& bar, uint8_t display) {
--     (void)display;
--     auto const& fire_style = Lemonbar::PowerlineStyle::fire;
--
--     // Load (depends on the amount of cores! I have 12 hw cores).
--     auto load_colors = Lemonbar::section_colors(cpu_load, 4.0f, 12.0f);
--     bar.separator(Lemonbar::Separator::left, load_colors.first, load_colors.second, fire_style);
--     bar().precision(2);
--     bar() << std::fixed << icon << ' ' << cpu_load << ' ';
--
--     // RAM usage.
--     auto neg_free_ram = -static_cast<int64_t>(free_ram);
--     auto neg_half_ram = -static_cast<int64_t>(total_ram) / 2;
--     auto memory_colors = Lemonbar::section_colors(neg_free_ram, neg_half_ram, 0L);
--     bar.separator(Lemonbar::Separator::left, memory_colors.first, memory_colors.second, fire_style);
--     print_used_memory(bar() << ' ', total_ram - free_ram, total_ram) << ' ';
--
--     bool is_first = true;
--     for(uint32_t temp : cpu_temps) {
--         auto style = is_first ? fire_style : Lemonbar::PowerlineStyle::none;
--         is_first = false;
--         auto temp_colors = Lemonbar::section_colors<uint32_t>(temp, 50, 90);
--         bar.separator(Lemonbar::Separator::left, temp_colors.first, temp_colors.second, style);
--         bar() << ' ' << temp << "\ufa03 ";
--     }
--     bar.separator(Lemonbar::Separator::left, Lemonbar::Coloring::white_on_black, fire_style);
-- }

print :: MVar Load.LoadState -> Index -> L.Powerlemon
print _ _ = return ()
