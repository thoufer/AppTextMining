#-------------------------------------------------------------------------
# IRTM Provided a list of "all applications" installed on machines across
# the service. Presumably this is from the belarc monitor software which
# only indexes applications that modify the registry.
#
#
#-------------------------------------------------------------------------
library(stringr)

# Read file from IRTM ----
#  (saved csv)
SoftwareList <- read.csv('Count all inventoried products and versions 2-8-17.csv',
                         header = TRUE,
                         as.is = TRUE)

# IRTM conducted frequencies based on the product version column. For our
# This is somewhat meaningless.  Lets drop the ProductVersion Column and
# recalculate frequencies based solely on the name
SoftwareListFreq <- aggregate(list(Count= SoftwareList$Count),
                              by= list(ProductName= SoftwareList$ProductName),
                              sum)

# Begin Name Munging ----
# Remove various punctuation
SoftwareListFreq$ProductName <- gsub("[<>?,;'!@#{}=_]+", "",
                                     SoftwareListFreq$ProductName)


# Remove leading and trailing spaces from any name.
SoftwareListFreq$ProductName <- str_trim(SoftwareListFreq$ProductName)


# Reduce the list by known standard applications installed on all machines
# or utilities available on apps to go.
# vendors/applications with no scientific value.
StandardList <- c('Google','Google Chrome','Chrome','Mozilla','Firefox','Adobe',
                  'Microsoft Office', 'Dell', 'Symantec', '7-Zip','McAfee',
                  'Pulse','KeePass','Vidyo','Bison Connect','Internet Explorer',
                  'Shoretel','VMWare','Acrobat')

StandardItems <- sapply(StandardList,
                      function(pattern, txt) {grep(pattern,
                                                   txt,
                                                   ignore.case = TRUE)
                      },
                      txt = SoftwareListFreq$ProductName ,
                      simplify = TRUE)

StandardItems <- unique(unlist(StandardItems))

SoftwareListFreq <- SoftwareListFreq[-StandardItems, ]

# Drop names with that contain some words known not to be scientific in nature
PatternList <- c('HP','Hewlett-Packard','Hewlett Packard','Hitachi','Deskjet',
                 'Designjet','laserjet','printer','Uninstaller','ThinkPad',
                 'Lotus','Rescue','Recovery','hard drive[s]','H&R','Modem',
                 'Keyboard','Driver[s]','Storage','Adapter','Ethernet',
                 'Amazon','Brother','Fujitsu','Wireless','Bluetooth',
                 'Sharepoint','Kyocera','kodak','Drobo','Epson','Ad-aware',
                 'Konica','Sketchup','NVIDIA*','Quickbooks*','Wacom',
                 'Nero','Cisco','WebEx','Flash','Citrix','YouTube','Quicken',
                 'Canon','RealTek','Firmware','AMD','WLAN', 'NetBackup',
                 'Toshiba','AOL','Seagate','Rioch','Logitech', 'Radeon','Xerox',
                 'Iomega','Norton','Novell','TurboTax','Nikon')

Items <- sapply(PatternList,
              function(pattern, txt) {grep(pattern,
                                           txt,
                                           ignore.case = TRUE)
              },
              txt = SoftwareListFreq$ProductName ,
              simplify = TRUE)

Items <- unlist(Items)
SoftwareListFreq <- SoftwareListFreq[-Items, ]

# Remove Windows Operating system stuff.
PatternList <- c('Windows Server','Windows 2000','Windows XP','Windows 2008',
                 'Microsoft Windows')

Items <- sapply(PatternList,
                function(pattern, txt) {grep(pattern,
                                             txt,
                                             ignore.case = TRUE)
                },
                txt = SoftwareListFreq$ProductName ,
                simplify = TRUE)

Items <- unlist(Items)
SoftwareListFreq <- SoftwareListFreq[-Items, ]



PatternList <- c('Graphics Controller','Chipset','Driver','Wifi','Roxio',
                 'Lexmark','F-Secure','Sandisk','media','Tetris','Pinball',
                 'Comcast', 'BIOS','Facebook','GoTo','^[0-9]+$',
                 'Gmail','Broadcom','PowerDVD','iphone','Juniper','Jabra',
                 'Lenovo','DisplayLink','RealPlayer','Intel Rapid Start',
                 'Creattive Software','QuickTime','Internet Information Server',
                 'Blizzard','Family Tree Maker','Avery','FedEx','DirectX',
                 'Excel','Webroot','DOI Learn','Free Fall Data Protection',
                 'Process Manager','Games','DivX','Benefits','ASUS','Paragon',
                 'Maxtor','Kaspersky','PCIe','Family Feud','PS/2','Costco',
                 'Pidgin','iTunes','APC','Microsoft Word','Skype','Bejeweled',
                 'Bing Bar','Fitbit','User Profile','Windows Error','SSD',
                 'Kies','Updater','Opera','Xprotect','Office XP','WinZip',
                 'DYMO','BlackBerry','Kazoo Player','Xbox','TrippLite',
                 'Memory Card','Malwarebytes','Retirement','Dragon',
                 'ipod','blasterball','Palm','Leapfrog','Winamp','Sound Blaster',
                 'Music','Rosetta','W32','AVG')

Items <- sapply(PatternList,
                function(pattern, txt) {grep(pattern,
                                             txt,
                                             ignore.case = TRUE)
                },
                txt = SoftwareListFreq$ProductName ,
                simplify = TRUE)

Items <- unlist(Items)
SoftwareListFreq <- SoftwareListFreq[-Items, ]

# Take a quick random look at what remains
nrow(SoftwareListFreq)
sample(unique(SoftwareListFreq$ProductName),1000)

