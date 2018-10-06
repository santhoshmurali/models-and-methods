use wideang4_mgn8;

select * from mgn_sales_order_status;

select orders.*, quotes.product_id, quotes.name, quotes.is_virtual, quotes.price, quotes.discount_percent from mgn_sales_flat_order as orders right outer join mgn_sales_flat_quote_item as quotes on orders.quote_id = quotes.quote_id ;

select * from mgn_sales_flat_order;

select * from mgn_sales_flat_quote_item;

select orders.increment_id, orders.customer_id, orders.customer_firstname, orders.customer_lastname, quotes.product_id, quotes.name, quotes.is_virtual, quotes.price, quotes.discount_percent from mgn_sales_flat_order as orders right outer join mgn_sales_flat_quote_item as quotes on orders.quote_id = quotes.quote_id 
where orders.customer_id is not null;


SELECT DISTINCT TABLE_NAME 
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE COLUMN_NAME IN ('customer_id')
        AND TABLE_SCHEMA='wideang4_mgn8';
        

select count(1) from mgn_sales_flat_order_item;
select * from mgn_sales_flat_order_item;
#'Order_id'
select count(1) from mgn_sales_flat_creditmemo;
#'billing_address_id'
select count(1) from mgn_sales_flat_shipment;


select item_id,quote_id,created_at,updated_at,product_id,name from  mgn_sales_flat_quote_item;

select * from
(
select 
orders.customer_id as Cust_Id,
orders.customer_firstname as Customer_Name,
orders.status as Order_Status,
orders.base_grand_total as Rentals,
orders.total_paid as RentalsPaid ,
orders.start_datetime as 'Start', 
orders.end_datetime as 'End',
orders.quote_id as QID,
sales.item_id as IID, 
sales.product_id as ProductID, 
sales.name as ProductName
from mgn_sales_flat_order as orders join
mgn_sales_flat_quote_item as sales on
orders.quote_id = sales.quote_id where
orders.status not in ('canceled','processing','pending','pending_payment')
) as core