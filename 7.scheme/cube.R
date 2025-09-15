
library(plotly)

# ---- 参数 ----
levels <- c(0, 0.5, 1)    # 三个格点
grid <- expand.grid(x=levels, y=levels, z=levels)

# ---- 明确构造网格线（每条线段之间用 NA 分隔） ----
# X方向的线（在每个 y,z 层）
x_x <- c(); x_y <- c(); x_z <- c()
for(y in levels) for(z in levels){
  x_x <- c(x_x, levels, NA)
  x_y <- c(x_y, rep(y, length(levels)), NA)
  x_z <- c(x_z, rep(z, length(levels)), NA)
}

# Y方向的线（在每个 x,z 层）
y_x <- c(); y_y <- c(); y_z <- c()
for(x in levels) for(z in levels){
  y_x <- c(y_x, rep(x, length(levels)), NA)
  y_y <- c(y_y, levels, NA)
  y_z <- c(y_z, rep(z, length(levels)), NA)
}

# Z方向的线（在每个 x,y 层）
z_x <- c(); z_y <- c(); z_z <- c()
for(x in levels) for(y in levels){
  z_x <- c(z_x, rep(x, length(levels)), NA)
  z_y <- c(z_y, rep(y, length(levels)), NA)
  z_z <- c(z_z, levels, NA)
}

# 合并所有网格线为一个 trace（效率更高）
gx <- c(x_x, y_x, z_x)
gy <- c(x_y, y_y, z_y)
gz <- c(x_z, y_z, z_z)

# ---- 示例轨迹 ----
red_traj   <- data.frame(x=c(0.5, 0.5, 1.0, 1.0),
                         y=c(0.0, 0.0, 0.0, 0.5),
                         z=c(0.5, 0.0, 0.0, 0.0))
green_traj <- data.frame(x=c(0.5, 0.0, 0.0, 0.0),
                         y=c(0.5, 0.5, 1.0, 1.0),
                         z=c(0.5, 0.5, 0.5, 1.0))
blue_traj  <- data.frame(x=c(0.5, 1.0, 1.0, 1.0),
                         y=c(1.0, 1.0, 1.0, 0.5),
                         z=c(1.0, 1.0, 0.5, 0.5))
red_traj_1   <- data.frame(x=c(0.5),
                         y=c(0.0),
                         z=c(0.5))
red_traj_2   <- data.frame(x=c(0.5, 0.5),
                         y=c(0.0, 0.0),
                         z=c(0.5, 0.0))
red_traj_3   <- data.frame(x=c(0.5, 0.5, 1.0),
                         y=c(0.0, 0.0, 0.0),
                         z=c(0.5, 0.0, 0.0))

red_traj_x   <- data.frame(x=c( 0.5, 1.0),
                         y=c(0.0, 0.0),
                         z=c(0.0, 0.0))
green_traj_x <- data.frame(x=c(0.5, 0.0),
                         y=c(0.5, 0.5),
                         z=c(0.5, 0.5))
blue_traj_x  <- data.frame(x=c(0.5, 1.0),
                         y=c(1.0, 1.0),
                         z=c(1.0, 1.0))
# ---- 绘图 ----
p_three_traj <- plot_ly() %>%
  # 网格线
  add_trace(x = gx, y = gy, z = gz,
            type = 'scatter3d', mode = 'lines',
            line = list(color = 'grey', width = 2),
            showlegend = FALSE) %>%
  # 所有小黑点（格点）
  add_trace(x = grid$x, y = grid$y, z = grid$z,
            type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, color = 'black'),
            showlegend = FALSE) %>%
  # 红色轨迹（粗线 + 大球）
  add_trace(x = red_traj$x, y = red_traj$y, z = red_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'red', width = 8),
            marker = list(size = 8, color = 'red'),
            name = 'Trajectory 1') %>%
  # 绿色轨迹
  add_trace(x = green_traj$x, y = green_traj$y, z = green_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'green', width = 8),
            marker = list(size = 8, color = 'green'),
            name = 'Trajectory 2') %>%
  # 蓝色轨迹
  add_trace(x = blue_traj$x, y = blue_traj$y, z = blue_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'blue', width = 8),
            marker = list(size = 8, color = 'blue'),
            name = 'Trajectory 3') %>%
  # 坐标轴、刻度、视角
  layout(
    scene = list(
      xaxis = list(title = 'Factor A',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      yaxis = list(title = 'Factor B',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      zaxis = list(title = 'Factor C',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      camera = list(eye = list(x = 1.6, y = 1.2, z = 0.7))
    ),
    margin = list(l=0, r=0, b=0, t=30)
  )

# 分步骤图
p_red_traj_3 <- plot_ly() %>%
  # 网格线
  add_trace(x = gx, y = gy, z = gz,
            type = 'scatter3d', mode = 'lines',
            line = list(color = 'grey', width = 2),
            showlegend = FALSE) %>%
  # 所有小黑点（格点）
  add_trace(x = grid$x, y = grid$y, z = grid$z,
            type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, color = 'black'),
            showlegend = FALSE) %>%
  # 红色轨迹（粗线 + 大球）
  add_trace(x = red_traj_3$x, y = red_traj_3$y, z = red_traj_3$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'red', width = 8),
            marker = list(size = 8, color = 'red')) %>%
  # 坐标轴、刻度、视角
  layout(
    scene = list(
      xaxis = list(title = 'Factor A',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      yaxis = list(title = 'Factor B',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      zaxis = list(title = 'Factor C',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      camera = list(eye = list(x = 1.6, y = 1.2, z = 0.7))
    ),
    margin = list(l=0, r=0, b=0, t=30)
  )


p_three_traj_EE <- plot_ly() %>%
  # 网格线
  add_trace(x = gx, y = gy, z = gz,
            type = 'scatter3d', mode = 'lines',
            line = list(color = 'grey', width = 2),
            showlegend = FALSE) %>%
  # 所有小黑点（格点）
  add_trace(x = grid$x, y = grid$y, z = grid$z,
            type = 'scatter3d', mode = 'markers',
            marker = list(size = 3, color = 'black'),
            showlegend = FALSE) %>%
  # 灰色轨迹一（粗线 + 大球）
  add_trace(x = red_traj$x, y = red_traj$y, z = red_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'darkgrey', width = 8),
            marker = list(size = 8, color = 'darkgrey')) %>%
  # 灰色轨迹二
  add_trace(x = green_traj$x, y = green_traj$y, z = green_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'darkgrey', width = 8),
            marker = list(size = 8, color = 'darkgrey')) %>%
  # 灰色轨迹三
  add_trace(x = blue_traj$x, y = blue_traj$y, z = blue_traj$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'darkgrey', width = 8),
            marker = list(size = 8, color = 'darkgrey')) %>%
  # 红色轨迹片段
  add_trace(x = red_traj_x$x, y = red_traj_x$y, z = red_traj_x$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'red', width = 8),
            marker = list(size = 8, color = 'red'),
            name = 'Trajectory 1 factor A') %>%
  # 绿色轨迹片段
  add_trace(x = green_traj_x$x, y = green_traj_x$y, z = green_traj_x$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'green', width = 8),
            marker = list(size = 8, color = 'green'),
            name = 'Trajectory 2 factor A') %>%
  add_trace(x = blue_traj_x$x, y = blue_traj_x$y, z = blue_traj_x$z,
            type = 'scatter3d', mode = 'lines+markers',
            line = list(color = 'blue', width = 8),
            marker = list(size = 8, color = 'blue'),
            name = 'Trajectory 3 factor A') %>%
  # 坐标轴、刻度、视角
  layout(
    scene = list(
      xaxis = list(title = 'Factor A',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      yaxis = list(title = 'Factor B',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      zaxis = list(title = 'Factor C',
                   tickmode = 'array', tickvals = levels, ticktext = as.character(levels)),
      camera = list(eye = list(x = 1.6, y = 1.2, z = 0.7))
    ),
    margin = list(l=0, r=0, b=0, t=30)
  )

p_three_traj_EE
