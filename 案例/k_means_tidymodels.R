library(tidymodels)

set.seed(27) #产生样本点需要用到

## 这里是设置参数

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)


## 通过上面的参数算出样本
temp <- centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points)
# rnorm(样本个数,样本均值)

## 函数 unnest()目的是将temp的x轴,y轴数据展开
labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)

points <- 
  labelled_points %>% 
  select(-cluster)

## kmeans是stats包里面的函数
kclust <- kmeans(points, centers = 3)
(kclust) ## 原始的kmeans返回的列表读起来非常糟糕

## tidymodels包里面的函数
## 对数据做预测 模型 + 原始数据
(augment(kclust, points))
## tidy是返回一个
(tidy(kclust))
(glance(kclust))


kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )
clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)+
  geom_point(data = clusters, size = 4, shape = "x")

## 选择最好的一个k值
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
