import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main :: IO ()
main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 0.5)]
                                  , graphLabel = Just "MEM"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 0.5)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "CPU"
                                  }
      pagerCfg = defaultPagerConfig { activeWindow = id
                                    }
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      pager = taffyPagerNew pagerCfg
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 2.0 memCallback
      cpu = pollingGraphNew cpuCfg 2.0 cpuCallback
      tray = systrayNew
      bat = batteryBarNew defaultBatteryConfig 10.0
  defaultTaffybar
    defaultTaffybarConfig { startWidgets = [ pager, note ]
                          , endWidgets   = [ clock, bat, tray, mem, cpu, mpris ]
                          , barHeight    = 20
                          }
