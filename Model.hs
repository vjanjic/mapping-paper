
time_dist = 0.01
time_gath = 0.01
time_copy = 0.01

cost_p = 171 -- 140
cost_f = 466 -- 409
cost_f' = 50 -- 110

cost_e = 104

cost_e2 =  40

k = 1024

map_workers = 16 
farm_workers = 8

cost_example_1 n = k * (cost_p + cost_f)
cost_example_2 n = k * (max cost_p cost_f)
cost_example_3 n = k * (max cost_p (cost_map map_workers n))
cost_example_4 n = k * (max (cost_farm farm_workers n) (cost_map map_workers n))

cost_map  m n = time_dist + ((cost_f' * m )/(if n < m then m else n {- max n m -} )) + time_gath
cost_farm m n = time_dist + (cost_p/(if n < m then m else n {-max m n -} )) + time_gath

-- cost_mapE m n = time_dist + ((cost_e * m )/(if n < m then m else n {- max n m -} )) + time_gath
cost_mapE m n = time_dist + (cost_e/((min m n))) + time_gath --  if n < m then m else n {-max m n -} )) + time_gath

cost_euler n = 10000 * (cost_farmE 24 n)

cost_farmE m n = time_dist + ((cost_e2  )/(min n m )) + time_gath

cost_euler2 n = 10000 * (cost_mapE 24 n)

speedup f n = (cost_example_1 n)/(f n)
speedup2 f n = (cost_e * 10000)/(f n)
