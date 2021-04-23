---------------------------------------------------------
-- Validation Queries for v5.0

select count(*)
from appendFullBlock;
select count(*)
from applyTransactions;
select count(*)
from createUtxoState;
select count(*)
from validateTxStateful;
select count(*)
from verifyScript;

select *
from appendFullBlock
where blockId = 'blockId';
select *
from applyTransactions
where blockId = 'blockId';
select *
from createUtxoState
where blockId = 'blockId';
select *
from validateTxStateful
where blockId = 'blockId';
select *
from verifyScript
where blockId = 'blockId';

-- delete from appendFullBlock where blockId = 'blockId';
-- delete from applyTransactions where blockId = 'blockId';
-- delete from createUtxoState where blockId = 'blockId';
-- delete from validateTxStateful where blockId = 'blockId';
-- delete from verifyScript where blockId = 'blockId';


-- tx with negative cost
select count(*)
from validateTxStateful
where cost < 0;

-- invalid tx_num
select b.blockId, t.tx_count as c, b.tx_num as n
from (select blockId, count(*) as tx_count
      from validateTxStateful
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
where c != n;

-- invalid tx cost
select b.blockId, t.sum_costs as sum_tx_costs, b.cost as block_cost
from (select blockId, sum(cost) as sum_costs
      from validateTxStateful
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
where sum_tx_costs != block_cost;

-- invalid script cost or time
select tx.blockId,
       tx.txId,
       t.script_count,
       t.sum_script_costs,
       tx.cost as tx_cost,
       t.sum_script_time,
       tx.time as tx_time
from (select blockId,
             txId,
             sum(cost) as sum_script_costs,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript
      group by blockId, txId) as t
         join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId
where sum_script_costs > tx_cost
   or sum_script_time > tx_time
order by script_count desc;

-- top tx_time vs script_time ratio
select tx.blockId,
       tx.txId,
       t.script_count,
       tx.time / 1000                       as tx_time_us,
       t.sum_script_time / 1000             as script_time_us,
       (tx.time - t.sum_script_time) / 1000 as time_diff,
       tx.time * 10 / t.sum_script_time     as time_ratio
from (select blockId,
             txId,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript
      group by blockId, txId) as t
         join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId
order by time_ratio desc;

-- count tx by tx_time vs script_time ratio
select t.time_ratio / 10,
       count(*),
       avg(t.tx_time_us),
       avg(t.script_time_us),
       avg(t.script_count)
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10 desc;

-- small ratio: count tx by tx_time vs script_time
select t.time_ratio,
       count(*),
       avg(t.tx_time_us),
       avg(t.script_time_us),
       avg(t.script_count)
from (select tx.blockId,
             tx.txId,
             t.script_count,
             tx.time / 1000                   as tx_time_us,
             t.sum_script_time / 1000         as script_time_us,
             tx.time * 10 / t.sum_script_time as time_ratio
      from (select blockId,
                   txId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId, txId) as t
               join validateTxStateful as tx on tx.blockId = t.blockId and tx.txId = t.txId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio desc;

-- top block_time vs script_time ratio
select b.blockId,
       t.script_count,
       b.time / 1000                       as block_time_us,
       t.sum_script_time / 1000            as script_time_us,
       (b.time - t.sum_script_time) / 1000 as time_diff,
       b.time * 10 / t.sum_script_time     as time_ratio
from (select blockId,
             sum(time) as sum_script_time,
             count(*)  as script_count
      from verifyScript
      group by blockId) as t
         join applyTransactions as b on b.blockId = t.blockId
order by time_ratio desc;

-- count blocks by block_time vs script_time ratio
select t.time_ratio / 10,
       count(*),
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.script_time_us), 0) as avg_script_time_us,
       round(avg(t.script_count), 1)   as avg_script_count
from (select b.blockId,
             t.script_count,
             b.time / 1000                       as block_time_us,
             t.sum_script_time / 1000            as script_time_us,
             (b.time - t.sum_script_time) / 1000 as time_diff,
             b.time * 10 / t.sum_script_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t
group by t.time_ratio / 10
order by t.time_ratio / 10 desc;

-- small ratio: count blocks by block_time vs script_time
select t.time_ratio,
       count(*),
       round(avg(t.block_time_us), 0)  as avg_block_time_us,
       round(avg(t.script_time_us), 0) as avg_script_time_us,
       round(avg(t.script_count), 1)   as avg_script_count
from (select b.blockId,
             t.script_count,
             b.time / 1000                       as block_time_us,
             t.sum_script_time / 1000            as script_time_us,
             (b.time - t.sum_script_time) / 1000 as time_diff,
             b.time * 10 / t.sum_script_time     as time_ratio
      from (select blockId,
                   sum(time) as sum_script_time,
                   count(*)  as script_count
            from verifyScript
            group by blockId) as t
               join applyTransactions as b on b.blockId = t.blockId) as t
where t.time_ratio <= 19
group by t.time_ratio
order by t.time_ratio desc;

-- v4.0: top 100 block costs vs total validation times
select b1.blockId,
       b1.height,
       b1.tx_num,
       b2.cost,
       b1.time / 1000                       as t1_us,
       b2.time / 1000                       as t2_us,
       b3.time / 1000                       as t3_us,
       (b1.time + b2.time + b3.time) / 1000 as time_us
from appendFullBlock as b1
         join applyTransactions as b2 on b1.blockId = b2.blockId
         join createUtxoState b3 on b1.blockId = b3.blockId
order by time_us desc
limit 1000;

-- count blocks where script time is less than 50%
select count(*)
from (select b1.blockId,
             b1.height,
             b1.tx_num,
             b2.cost,
             b1.time / 1000                       as t1_us,
             b2.time / 1000                       as t2_us,
             b3.time / 1000                       as t3_us,
             (b1.time + b2.time + b3.time) / 1000 as time_us
      from appendFullBlock as b1
               join applyTransactions as b2 on b1.blockId = b2.blockId
               join createUtxoState b3 on b1.blockId = b3.blockId
      where t3_us >= t2_us
      order by b2.cost desc);


-- new vs old
select at5.blockId, at5.time as t5, at4.time as t4
from applyTransactions as at5
         join applyTransactions4 as at4 on at5.blockId = at4.blockId
where at5.time > at4.time
