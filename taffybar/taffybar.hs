import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU2
import System.Information.CPU

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

tempCallback :: IO [Double]
tempCallback = do
  t <- read <$> readFile "/sys/devices/virtual/thermal/thermal_zone1/temp"
  return [t/100000]

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
      tempCfg = defaultGraphConfig { graphDataColors = [ (1, 1, 0, 0.5)
                                                       , (1, 0, 1, 0.5)
                                                       ]
                                  , graphLabel = Just "TEMP"
                                  }
      pagerCfg = defaultPagerConfig { activeWindow = id
                                    }
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      pager = taffyPagerNew pagerCfg
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 2.0 memCallback
      cpu = pollingGraphNew cpuCfg 2.0 cpuCallback
      temp = pollingGraphNew tempCfg 2.0 tempCallback
      tray = systrayNew
      bat = batteryBarNew defaultBatteryConfig 10.0
  defaultTaffybar
    defaultTaffybarConfig { startWidgets = [ pager, note ]
                          , endWidgets   = [ clock, bat, tray, temp, mem, cpu, mpris ]
                          , barHeight    = 20
                          }
