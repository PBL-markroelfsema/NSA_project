ggplot(data=NoPolicy$EfficiencyFleet_new_cars) +
       geom_line(aes(x=year, y=value, colour=region)) +
       geom_point(data=filter(NoPolicy$EfficiencyFleet_new_cars, region=="World"),aes(x=year, y=value)) +
       theme_bw()

ggplot(data=NoPolicy$EfficiencyFleet_new_busses) +
  geom_line(aes(x=year, y=value, colour=region)) +
  geom_point(data=filter(NoPolicy$EfficiencyFleet_new_busses, region=="World"),aes(x=year, y=value)) +
  theme_bw()

ggplot(data=NoPolicy$EfficiencyFleet_new_MedT) +
  geom_line(aes(x=year, y=value, colour=region)) +
  geom_point(data=filter(NoPolicy$EfficiencyFleet_new_MedT, region=="World"),aes(x=year, y=value)) +
  theme_bw()

ggplot(data=NoPolicy$EfficiencyFleet_new_HvyT) +
  geom_line(aes(x=year, y=value, colour=region)) +
  geom_point(data=filter(NoPolicy$EfficiencyFleet_new_HvyT, region=="World"),aes(x=year, y=value)) +
  theme_bw()
