# `log-stats` - a command-line data analyzer thing

While trying to overclock my cpu, I wanted to be able to get the
general gist of the temperature values, power consumption, VCore,
etc. while stress-testing, but grew tired of repeatedly typing
```
$ awk '/power1/ { t += $2; n++} END { print t/n }' sensor_log.txt
$ grep power1 sensor_log.txt | sort -nrk2,2 | head -1
```
and variations on these.

You provide the program with a period, in lines, after which the
log repeats, and a set of statistical operations to perform on 
each of the numbers in the input.

## Some examples:
The basic format is `log-stats <period> <statistical operation...>`:
```
$ cat ex1.txt
The numbers are 12 and 34.56.
The number 3 is constant throughout.
The numbers are 11 and 12.02.
The number 3 is constant throughout.
The numbers are 14 and 2.48.
The number 3 is constant throughout.
The numbers are 17 and 17.33.
The number 3 is constant throughout.
The numbers are 2 and 14.57.
The number 3 is constant throughout.
$ log-stats 2 mean <ex1.txt
The numbers are 11 and 16.19.
The number 3 is constant throughout.
```
It respects decimal precision of the inputs.
You can supply more than one function to do:
```
$ log-stats 2 min max <ex1.txt
The numbers are 2 and 2.48.
The numbers are 17 and 34.56.
The number 3 is constant throughout.
```
You'll notice that it only lists the second line once, because it
is the same under each of the provided functions. (This feature
is most useful when there's blank lines, or headers/titles, which
are always the same and you don't want to see too much of them.)

It supports variable amounts of whitespace:
```
$ cat ex2.txt
This number is space-padded:        12.5
This number is space-padded:       217.8
This number is space-padded:         4.5
This number is space-padded:      1234.5
This number is space-padded:        14.7
$ log-stats 1 median max <ex2.txt
This number is space-padded:        14.7
This number is space-padded:      1234.5
```
It defaults to `1` when no period is provided:
```
$ log-stats min <ex2.txt
This number is space-padded:         4.5
```
And it defaults to `mean` when no statistical operation is provided:
```
$ log-stats <ex2.txt
This number is space-padded:        296.8
```

Here's a real-life examples:
```
$ while :; do date; sensors; grep MHz /proc/cpuinfo; sleep 1; done >sensor_log.txt # I stopped it at some point
$ (date; sensors; grep MHz /proc/cpuinfo) | wc -l # Find how many lines per repetition
34
$ log-stats 34 mean max <sensor_log.txt
Sun Dec  3 18:12:29 EST 2017
Sun Dec  3 18:14:59 EST 2017
k10temp-pci-0c3
Adapter: PCI adapter
temp1:        +42.7°C  (high = +70.0°C)
temp1:        +47.0°C  (high = +70.0°C)
                       (crit = +83.5°C, hyst = +80.5°C)

atk110-acpi-0
Adapter: ACPI interface
Vcore Voltage:      +1.20 V  (min =  +0.80 V, max =  +1.60 V)
Vcore Voltage:      +1.30 V  (min =  +0.80 V, max =  +1.60 V)
+3.3V Voltage:      +3.30 V  (min =  +2.97 V, max =  +3.63 V)
+5V Voltage:        +4.99 V  (min =  +4.50 V, max =  +5.50 V)
+5V Voltage:        +5.01 V  (min =  +4.50 V, max =  +5.50 V)
+12V Voltage:      +11.97 V  (min = +10.20 V, max = +13.80 V)
+12V Voltage:      +12.03 V  (min = +10.20 V, max = +13.80 V)
CPU Fan Speed:     2453 RPM  (min =  600 RPM, max = 7200 RPM)
CPU Fan Speed:     2667 RPM  (min =  600 RPM, max = 7200 RPM)
Chassis Fan Speed: 1808 RPM  (min =  600 RPM, max = 7200 RPM)
Chassis Fan Speed: 1824 RPM  (min =  600 RPM, max = 7200 RPM)
CPU Temperature:    +54.0°C  (high = +60.0°C, crit = +95.0°C)
CPU Temperature:    +60.0°C  (high = +60.0°C, crit = +95.0°C)
MB Temperature:     +31.5°C  (high = +45.0°C, crit = +75.0°C)
MB Temperature:     +32.0°C  (high = +45.0°C, crit = +75.0°C)

amdgpu-pci-100
Adapter: PCI adapter
fan1:         335 RPM
fan1:         336 RPM
temp1:        +39.8°C  (crit =  +0.0°C, hyst =  +0.0°C)
temp1:        +41.0°C  (crit =  +0.0°C, hyst =  +0.0°C)

fam15h_power-pci-0c4
Adapter: PCI adapter
power1:       93.24 W  (crit =  95.13 W)
power1:       96.93 W  (crit =  95.13 W)

cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
cpu MHz		: 3600.000
```

