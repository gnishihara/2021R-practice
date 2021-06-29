# 2021 R 研修会
# Greg Nishihara
# 2021 / 06 / 29
################################################

# Load packages
# CTRL + D 一行削除
# CTRL + ALT + ↑ / CTRL + ALT + ↓ : カーソル複製
library(tidyverse)
library(lubridate)
library(gnnlab)
library(magick)
library(showtext)
library(readxl)

# Read data file
filename = "~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"
esheet = excel_sheets(filename)

d1 = read_xlsx(filename, sheet = esheet[1])
d2 = read_xlsx(filename, sheet = esheet[2])

# d1 の構造を修正する
# ...5, ...10, などの列を削除する
# 正規表現で検索している

d1 = d1 |> select(matches("ライン|距離|時刻"))

d1l5 = d1 |> select(matches("[5]$"))

d1l5 = d1l5 |>
  mutate(amamo = 
           ifelse(is.na(`時刻5`),
                  "なし", "あり"))

d1l7 = d1 |> select(matches("[7]$"))

d1l7 = d1l7 |>
  mutate(amamo = 
           ifelse(is.na(`時刻7`),
                  "なし", "あり"))

ggplot(d1l5) +
  geom_point(aes(x = `ライン5`,
                 y = amamo))

ggplot(d1l7) +
  geom_point(aes(x = `ライン7`,
                 y = amamo))

ggplot() +
  geom_point(aes( x= `ライン5`,
                  y = amamo,
                  color = "L5"),
             data = d1l5) +
  geom_point(aes( x= `ライン7`,
                  y = amamo,
                  color = "L7"),
             data = d1l7)
#####################################

# ここでエラー、縦にするには、つなげたい
# 列の種類は同じじゃないといけない
d1 |> pivot_longer(cols = everything())


# 列の種類を統一する

d1 |> 
  select(matches("ライン"))

d1 |> 
  select(matches("時刻")) # 種類がまざっている

d1 |> 
  select(matches("距離"))

# データの処理のやり直し（効率のいい方法）
# Sheet の読み直し
# すべての列を text としてよみこむ

d1 = read_xlsx(filename, sheet = esheet[1],
               col_types = "text")
d2 = read_xlsx(filename, sheet = esheet[2],
               col_types = "text")

# d1 からは必要な列だけ抽出する
d1 = d1 |> select(matches("ライン|距離|時刻|アマモ"))

# 横長の d1 を縦長に変換する
d1 = d1 |> pivot_longer(cols = everything())
d1

d1 = d1 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |> 
  mutate(line = as.numeric(line))

d1 = d1 |> 
  mutate(name = str_remove(name, "[0-9]+$"))
d1 |> tail() 

d1 = d1 |> pivot_wider(names_from = name,
                  values_from = value) |> 
  unnest(everything())

d1 = d1 |> 
  mutate(距離 = str_extract(距離, "[0-9]+")) |> 
  mutate(距離 = as.numeric(距離)) |> 
  mutate(アマモ = factor(アマモ))

ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d1)


d1b = d1 |> 
  mutate(line = factor(line)) |> 
  mutate(アマモ = str_remove(アマモ,
                             "orB"))
ggplot() + 
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d1b)

ggplot() + 
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d1b) +
  facet_grid(rows = vars(line))

ggplot() + 
  geom_point(aes(y = 距離,
                 x = line,
                 color = アマモ),
             data = d1b)

# Plots
color = viridis::viridis(6)
font_add_google("Noto Sans JP", "notosans-jp")
font_add_google("Noto Sans", "notosans")
showtext_auto()

p1 = ggplot() + 
  geom_tile(aes(y = 距離,
                 x = line,
                 fill = アマモ),
             data = d1b) +
  annotate(geom = "text",
           x = 1, 
           y = 166,
           label = "Seawall side",
           vjust = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 6, 
           y = 166,
           label = "Entrance to seagrass meadow",
           vjust = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170, 100),
                     breaks = seq(170, 100, 
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage",
                    values = rev(color[-6])) +
  theme_gray(base_family = "notosans")
p1   
wh = aseries(6)
plotname = "kabeyama_01.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1], units = "mm")

image_read_pdf(plotname) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname, "pdf", "png"))


##################################
d2 = d2 |> select(matches("ライン|距離|時刻|アマモ"))

# 横長の d1 を縦長に変換する
d2 = d2 |> pivot_longer(cols = everything())
d2 = d2 |> 
  mutate(line = str_extract(name, "[0-9]+$")) |> 
  mutate(line = as.numeric(line))

d2 = d2 |> 
  mutate(name = str_remove(name, "[0-9]+$"))
d2 |> tail() 

d2 = d2 |> pivot_wider(names_from = name,
                       values_from = value) |> 
  unnest(everything())

d2 = d2 |> 
  mutate(距離 = str_extract(距離, "[0-9]+")) |> 
  mutate(距離 = as.numeric(距離)) |> 
  mutate(アマモ = factor(アマモ))

ggplot() +
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d2)

d2b = d2 |> 
  mutate(line = factor(line)) |> 
  mutate(アマモ = str_remove(アマモ,
                             "orC")) |> 
  mutate(アマモ = ifelse(str_detect(アマモ, "^T$"), NA, アマモ))

ggplot() + 
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d2b)

ggplot() + 
  geom_point(aes(x = 距離,
                 y = アマモ,
                 color = line),
             data = d2b) +
  facet_grid(rows = vars(line))

ggplot() + 
  geom_point(aes(y = 距離,
                 x = line,
                 color = アマモ),
             data = d2b)

# Plots
color = viridis::viridis(6)
font_add_google("Noto Sans JP", "notosans-jp")
font_add_google("Noto Sans", "notosans")
showtext_auto()

p2 = ggplot() + 
  geom_tile(aes(y = 距離,
                x = line,
                fill = アマモ),
            data = d2b) +
  annotate(geom = "text",
           x = 1, 
           y = 166,
           label = "Seawall side",
           vjust = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 11, 
           y = 166,
           label = "Entrance to seagrass meadow",
           vjust = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170, 100),
                     breaks = seq(170, 100, 
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage", values = rev(color[-6])) +
  theme_gray(base_family = "notosans")
p2 
wh = aseries(6)
plotname = "kabeyama_02.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1], units = "mm")

image_read_pdf(plotname) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname, "pdf", "png"))



d1b |> mutate(Transect)

dall = bind_rows(`5` = d1b, 
                 `6` = d2b, .id = "month") |> 
  select(month, line, 距離, アマモ)

ggplot() + 
  geom_tile(aes(y = 距離,
                x = line,
                fill = アマモ),
            data = dall) +
  annotate(geom = "text",
           x = 1, 
           y = 166,
           label = "Seawall side",
           vjust = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 11, 
           y = 166,
           label = "Entrance to seagrass meadow",
           vjust = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170, 100),
                     breaks = seq(170, 100, 
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage",
                    values = rev(color[-6])) +
  theme_gray(base_family = "notosans") +
  facet_grid(rows = vars(month))



#

d1b = d1b |> mutate(line = factor(line,
                            levels = 1:11,
                            labels = 1:11))
d2b = d2b |> mutate(line = factor(line,
                                  levels = 1:11,
                                  labels = 1:11))

dall = bind_rows(`5` = d1b, 
                 `6` = d2b, .id = "month") |> 
  select(month, line, 距離, アマモ)

ggplot() + 
  geom_tile(aes(y = 距離,
                x = line,
                fill = アマモ),
            data = dall) +
  annotate(geom = "text",
           x = 1, 
           y = 166,
           label = "Seawall side",
           vjust = 0, hjust = 0,
           color = "white") +
  annotate(geom = "text",
           x = 11, 
           y = 166,
           label = "Entrance to seagrass meadow",
           vjust = 0, hjust = 1,
           color = "white") +
  scale_y_continuous(name = "Distance (m)",
                     trans = "reverse",
                     limits = c(170, 100),
                     breaks = seq(170, 100, 
                                  by = -10)) +
  scale_x_discrete(name = "Transect") +
  scale_fill_manual(name = "Coverage",
                    values = rev(color[-6])) +
  theme_gray(base_family = "notosans") +
  facet_grid(rows = vars(month))


