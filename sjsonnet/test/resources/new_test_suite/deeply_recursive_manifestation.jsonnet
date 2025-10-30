local reduce(func, array, default) =
  //  assert std.isFunction(func) : error 'reduce(func=...) should be a `function`';
  //  assert std.isArray(array) : error 'reduce(array=...) should be an `array`';
  //  assert std.length(array) > 0 : error 'reduce(array=...) should be a non-0 `array`';
  local aux(func, array, current, index) =
    if index < 0 then
      current
    else
      aux(func, array, func(array[index], current), index - 1) tailstrict;
  local length = std.length(array);
  if length == 0 then
    default
  else
    aux(func, array, array[length - 1], length - 2);

local visit(
  value,
  visitor,
  ctx,
  ctx_reducer,
      ) =
  local new_item = if std.isObject(value) then
    local merged = {
      [prop]: visit(value[prop], visitor, ctx, ctx_reducer)
      for prop in std.objectFields(value)
    };

    local new_item = {
      [prop]: merged[prop].item
      for prop in std.objectFields(merged)
    };
    local new_ctx = reduce(ctx_reducer, std.map(
      function(item) item.ctx,
      std.objectValues(merged)
    ), ctx);

    {
      item: new_item,
      ctx: new_ctx,
    }
  else if std.isArray(value) then
    local merged = std.map(
      function(item) visit(item, visitor, ctx, ctx_reducer),
      value,
    );

    // If we had a way to tee collections, that would be great... :(
    local new_item = std.map(function(item) item.item, merged);
    local all_ctx = std.map(function(item) item.ctx, merged);

    // Reduce all of the contexts
    local new_ctx = reduce(ctx_reducer, all_ctx, ctx);

    {
      item: new_item,
      ctx: new_ctx,
    }
  else
    {
      item: value,
      ctx: ctx,
    };
  local replacement = visitor(new_item.ctx, new_item.item);
  if replacement != null then
    local new_ctx = ctx_reducer(replacement.ctx, new_item.ctx);

    {
      item: replacement.item,
      ctx: new_ctx,
    }
  else
    new_item;


local routes = {
  '/inc/users/whoami': {
    get: {
      responses: {
        '200': {
          content: {
            'application/json': {
              schema: {
                properties: {
                  user: {
                    properties: {
                      '#id': 'the id this entity',
                      id: {
                        format: 'uuid',
                        type: 'string',
                        'x-name': 'users.User',
                      },
                    },
                    type: 'object',
                  },
                },
              },
            },
          },
        },
      },
    },
  },
};

visit(routes, function(ctx, item) { ctx: ctx, item: item }, {}, function(left, right) left + right)
