(** Saga pattern implementation using a routing-slip approach.

    This library implements the Saga pattern for managing distributed
    transactions without traditional two-phase commit. Instead of holding
    locks across services, a saga splits work into individual activities
    whose effects can be compensated (reversed) when later steps fail.

    Key components:
    - {!Activity}: encapsulates a [do_work] / [compensate] pair;
    - {!Work_item}: a unit of work, paired with its activity factory and
      arguments;
    - {!Work_log}: record of a completed step, used for compensation;
    - {!Routing_slip}: the document flowing through the saga;
    - {!Activity_host}: processes routing-slip messages addressed to a
      specific activity;
    - {!Activity_resolver}: turns activity type names into factories on
      deserialization;
    - {!Serializable}: JSON-friendly mirror of {!Routing_slip}, suitable
      for transmission over a message bus.

    See also: https://vasters.com/archive/Sagas.html *)

module Saga_types = Saga_types
module Work_item_arguments = Work_item_arguments
module Work_result = Work_result
module Activity = Activity
module Work_item = Work_item
module Work_log = Work_log
module Routing_slip = Routing_slip
module Activity_resolver = Activity_resolver
module Activity_host = Activity_host
module Serializable = Serializable
module Fallback_activity = Fallback_activity
module Parallel_activity = Parallel_activity
