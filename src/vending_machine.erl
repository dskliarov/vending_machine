%% @doc Manages an inventory of the vending machine
%% Inventory is storing in dets to allow the machine
%% to survive a power outage
%%
%% Available states:
%% =============================
%% - maintenance
%% - out_of_order
%% - ready_to_serve
%% - money_deposited
%%
%% Valid operations in each state
%%
%% maintenance state
%% =============================
%% - load_inventory
%% - set_price
%% - available_inventory
%% - lock
%%
%% out_of_order_state state
%% =============================
%% - unlock
%%
%% ready_to_serve state
%% =============================
%% - deposit
%% - refund
%% - select_item
%% - complete_transaction
%%
%% Following actions are changing states
%%
%% unlock
%% =============================
%% - to maintenance state
%%
%% lock
%% =============================
%% - to ready_to_serve
%% - to out_of_order
%%
%% @end

-module(vending_machine).
-behaviour(gen_statem).
-define(NAME, vending_machine).
-define(INVENTORY, inventory).
-define(CASH_REGISTER, cash_register).

-export([start_link/1]).

-export([init/1,
         callback_mode/0,
         terminate/3]).

-export([maintenance/3,
         out_of_order/3,
         ready_to_serve/3,
         money_deposited/3]).

-export([unlock/0,
         lock/0,
         initial_load/0,
         load_inventory/3,
         set_price/2,
         available_inventory/0,
         withdraw_cash/0,
         clear_machine/0,
         coffee/0,
         tea/0,
         candy/0,
         chips/0,
         deposit/1,
         refund/0,
         get_item/1]).
%%------------------------------------------------------------------------------
%%
%% gen_server API
%%
%%------------------------------------------------------------------------------

start_link(Options) ->
  gen_statem:start_link({local,?NAME}, ?MODULE, [], Options).

%%------------------------------------------------------------------------------
%%
%% Admin API
%%
%%------------------------------------------------------------------------------

unlock() ->
  gen_statem:cast(?NAME, unlock).

lock() ->
  gen_statem:cast(?NAME, lock).

load_inventory(ItemName, Qty, Price) ->
  gen_statem:call(?NAME, {load_inventory, ItemName, Qty, Price}).

set_price(ItemName, Price) ->
  gen_statem:call(?NAME, {set_price, ItemName, Price}).

available_inventory() ->
  gen_statem:cast(?NAME, available_inventory).

withdraw_cash() ->
  gen_statem:call(?NAME, withdraw_cash).

clear_machine() ->
  gen_statem:cast(?NAME, clear_machine).

initial_load() ->
  Items = [{tea, 10, 100}, {coffee, 10, 105}, {candy, 10, 120}, {chips, 10, 110}],
  gen_statem:call(?NAME, {load_inventory, Items}).

%%------------------------------------------------------------------------------
%%
%% User API
%%
%%------------------------------------------------------------------------------

coffee() -> get_item(coffee).

tea() -> get_item(tea).

candy() -> get_item(candy).

chips() -> get_item(chips).

deposit(Amount) ->
  gen_statem:call(?NAME, {deposit, Amount}).

refund() ->
  gen_statem:call(?NAME, refund).

get_item(ItemName) ->
  gen_statem:call(?NAME, select_item, ItemName).

%%------------------------------------------------------------------------------
%%
%% callbacks
%%
%%------------------------------------------------------------------------------

init([]) ->
  InventoryTable = open_db(?INVENTORY),
  CashRegister = open_db(?CASH_REGISTER),
  State = #{inventory => InventoryTable, register => CashRegister},
  IsInventoryAvailable = is_inventory_available(InventoryTable),
  InitialMachineState = initial_machine_state(IsInventoryAvailable),
  {ok, InitialMachineState, State}.

callback_mode() ->
  state_functions.

terminate(_Reason, _State, _Data) ->
  ok.

%%------------------------------------------------------------------------------
%%
%% state machine
%%
%%------------------------------------------------------------------------------

maintenance(enter, _OldState, State) ->
  print_maintenance(),
  {keep_state, State};

maintenance({call, From}, {set_price, ItemName, Price}, State) ->
  #{inventory := Table} = State,
  update_inventory_price(Table, ItemName, Price),
  Inventory = available_inventory(Table),
  {next_state, maintenance, State, [{reply, From, Inventory}]};

maintenance({call, From}, {load_inventory, ItemName, Qty, Price}, State) ->
  #{inventory := Table} = State,
  load_inventory(Table, ItemName, Qty, Price),
  Inventory = available_inventory(Table),
  {next_state, maintenance, State, [{reply, From, Inventory}]};

maintenance({call, From}, {load_inventory, Items}, State) when is_list(Items) ->
  #{inventory := Table} = State,
  Fun = fun({ItemName, Qty, Price}) -> load_inventory(Table, ItemName, Qty, Price) end,
  lists:foreach(Fun, Items),
  Inventory = available_inventory(Table),
  {next_state, maintenance, State, [{reply, From, Inventory}]};

maintenance(cast, clear_machine, State) ->
  #{inventory := InventoryTable, register := RegisterTable} = State,
  clear(InventoryTable),
  clear(RegisterTable),
  {next_state, maintenance, State};

maintenance(cast, lock, State) ->
  #{inventory := Table} = State,
  IsInventoryAvailable = is_inventory_available(Table),
  MachineState = initial_machine_state(IsInventoryAvailable),
  {next_state, MachineState, State}.

%%------------------------------------------------------------------------------

out_of_order(enter, _OldState, State) ->
  print_out_of_order(),
  {keep_state, State};

out_of_order(cast, unlock, State) ->
  {next_state, maintenance, State}.

%%------------------------------------------------------------------------------

ready_to_serve(enter, _OldState, State) ->
  print_ready(),
  {keep_state, State};

ready_to_serve(cast, available_inventory, State) ->
  #{inventory := Table} = State,
  Inventory = available_inventory(Table),
  print_inventory(Inventory),
  {next_state, ready_to_serve, State};

ready_to_serve({call, From}, {deposit, Amount}, State) ->
  print_money_deposited(Amount),
  State1 = maps:put(deposit, State, Amount),
  {next_state, money_deposited, State1, [{reply, From, {ok, #{deposit => Amount}}}]}.

%%------------------------------------------------------------------------------

money_deposited(enter, _OldState, State) ->
  print_money_deposited(),
  {keep_state, State};

money_deposited(cast, available_inventory, State) ->
  #{inventory := Table} = State,
  Inventory = available_inventory(Table),
  print_inventory(Inventory),
  {next_state, money_deposited, State};

money_deposited({call, From}, {deposit, Amount}, State) ->
  DepositSoFar = maps:get(deposit, State, 0),
  DepositSoFar1 = DepositSoFar + Amount,
  State1 = maps:put(deposit, State, DepositSoFar1),
  print_money_deposited(DepositSoFar1),
  {next_state, money_deposited, State1, [{reply, From, {ok, #{deposit => DepositSoFar1}}}]};

money_deposited({call, From}, refund, State) ->
  #{inventory := Table} = State,
  Refund = maps:get(deposit, State, 0),
  State1 = maps:put(deposit, State, 0),
  IsInventoryAvailable = is_inventory_available(Table),
  StateName = initial_machine_state(IsInventoryAvailable),
  {next_state, StateName, State1, [{reply, From, {ok, #{refund => Refund}}}]};

money_deposited({call, From}, {select_item, ItemName}, State) ->
  #{inventory := Table} = State,
  DepositSoFar = maps:get(deposit, State, 0),
  case get(Table, ItemName) of
    {error, not_found} ->
      print_item_unavailable(ItemName),
      {next_state, money_deposited, State, [{reply, From, {error, not_found}}]};
    {ItemName, Qty, Price} when DepositSoFar > Price ->
      load_inventory(Table, ItemName, Qty - 1, Price),
      IsInventoryAvailable = is_inventory_available(Table),
      StateName = initial_machine_state(IsInventoryAvailable),
      Result = #{item => ItemName, change => DepositSoFar - Price},
      State1 = maps:put(deposit, State, 0),
      {next_state, StateName, State1, [{reply, From, {ok, Result}}]};
    {ItemName, _Qty, Price} ->
      print_deposit_more_money(DepositSoFar - Price),
      {next_state, money_deposited, State, [{reply, From, {error, deposit_more}}]}
  end.

%%------------------------------------------------------------------------------
%%
%% Storage
%%
%%------------------------------------------------------------------------------

open_db(TableName) ->
  dets:open_file(TableName, [{type, set}]).

load_inventory(Table, ItemName, Qty, Price) ->
  InventoryItem = get(Table, ItemName),
  Qty1 = update_inventory_qty(InventoryItem, Qty),
  Payload = #{qty => Qty1, price => Price},
  dets:insert_new(Table, {ItemName, Payload}).

update_inventory_price(Table, ItemName, Price) ->
  InventoryItem = get(Table, ItemName),
  {Qty, Price1} = update_inventory_price(InventoryItem, Price),
  Payload = #{qty => Qty, price => Price1},
  dets:insert_new(Table, {ItemName, Payload}).

update_inventory_price({_ItemName, Qty, _OldPrice}, Price) ->
  {Qty, Price};
update_inventory_price({error, not_found}, Price) ->
  {0, Price}.

update_inventory_qty({_ItemName, Qty, _Price}, Qty1) ->
  Qty + Qty1;
update_inventory_qty({error, not_found}, Qty) ->
  Qty.

get(Table, ItemName) ->
  case dets:lookup(Table, ItemName) of
    [{_, #{qty := Qty}} = Item] when Qty > 0 ->
      inventory_item(Item);
    _ ->
      {error, not_found}
  end.

available_inventory(Table) ->
  ReadAll = ets:fun2ms(fun inventory_item/1),
  dets:select(Table, ReadAll).

is_inventory_available(Table) ->
  SelectPosQty = [{{'_',#{qty => '$1'}},[{'>', '$1', 0}],['$1']}],
  case dets:select(Table, SelectPosQty, 1) of
    {_Qty, _} ->
      true;
    _ ->
      false
  end.

initial_machine_state(false) ->
  out_of_order;
initial_machine_state(true) ->
  ready_to_serve.

clear(Table) ->
  dets:delete_all_objects(Table).

inventory_item({ItemName,
                #{qty := Qty, price := Price}}) ->
  {ItemName, Qty, Price}.

%%------------------------------------------------------------------------------
%%
%% Printer
%%
%%------------------------------------------------------------------------------

print_item_unavailable(ItemName) ->
  io:format("Item ~s is unavailable, please select another item~n", [ItemName]).

print_inventory_line({InventoryName, Qty, Price}) ->
  io:format("~s - ~w x ~wcents", [InventoryName, Qty, Price]).

print_inventory(Inventory) ->
 lists:foreach(fun print_inventory_line/1, Inventory).

print_deposit_more_money(Amount) ->
  io:format("Please deposite ~wcents more~n", [Amount]).

print_money_deposited() ->
  io:format("Please deposit money~n", []).

print_money_deposited(Amount) when Amount > 0  ->
  io:format("Deposited ~wcents~nPlease pick the item or deposit more money~n", [Amount]);
print_money_deposited(_Amount) ->
  print_money_deposited().

print_maintenance() ->
  L1 = "Machine is maintenance state",
  L2 = "To load inventory run: ",
  L3 = "vending_machine:load_inventory(ItemName, Qty, Price)",
  L4 = "vending_machine:initial_load()",
  L5 = "vending_machine:set_price(ItemName, Price)",
  io:format("~s;~n~s~n;~s~n;~s~n;~s~n", [L1, L2, L3, L4, L5]).

print_ready() ->
  L1 = "Machine is ready to serve you",
  L2 = "Please deposit some money",
  L3 = "vending_machine:help() for help",
  io:format("~s;~n~s~n;~s~n", [L1, L2, L3]).

print_out_of_order() ->
  L1 = "Machine is out of order",
  L2 = "vending_machine:admin_help() for help",
  io:format("~s;~n~s~n", [L1, L2]).
