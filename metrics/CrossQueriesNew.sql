-- count recorded common rows (v4 and v5)
select *, count_rows5 - count_rows4 as v5_v4_rows_diff
from (select count(*) as count_rows5 from applyTransactions),
     (select count(*) as count_rows4 from applyTransactions4),
     (select count(*) as common_rows
      from applyTransactions as t5
               join applyTransactions4 t4
                    on t5.blockId = t4.blockId);

-- group and count blocks by speedup range
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
group by speedup
order by speedup;

-- count blocks with speedup >= 2
select sum(num_blocks) from (
select t4.time / t5.time as speedup, count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where speedup >= 2
group by speedup
order by speedup
)
;

-- count blocks and average speedup for each range
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*)                                      as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time / t5.time >= 1
group by range
order by range;


-- outliers which are slower in v5, can be the result of measuring fluctuations
select sum(num_blocks) from (
select t4.time * 10 / t5.time as ratio,
       count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where ratio < 10
  and t4.height > 400000 -- among resent blocks
group by ratio
order by ratio
)
;

-- blocks with least 1.2 speedup in v5
select sum(num_blocks) from (
select t5.height / 100000                            as range,
       round(avg(t4.time * 100 / t5.time * 0.01), 2) as avg_speedup,
       count(*) as num_blocks
from applyTransactions t5 join applyTransactions4 t4 on t5.blockId = t4.blockId
where t4.time * 100 / t5.time * 0.01 >= 1.2
group by range
order by range
)
;


