# By Frank Leenaars
# University of Twente - Department of Instructional Technology
"use strict"

window.gearsketch = {}

# ---------------------------
# ---------- Util -----------
# ---------------------------
class Util
  # -- constants --
  @MODULE: 6
  @AXIS_RADIUS: 1.5 * @MODULE
  @MIN_GEAR_TEETH: 8
  @MIN_STACKED_GEARS_TEETH_DIFFERENCE: 4
  @SNAPPING_DISTANCE: 2 * @MODULE
  @EPSILON = 0.0001

  # -- enums --
  @Direction:
    CLOCKWISE: "clockwise"
    COUNTER_CLOCKWISE: "counterclockwise"

  @Side:
    LEFT: "left"
    RIGHT: "right"
    ON_LINE: "on line"

  @addPoints: () ->
    point = {x: 0, y: 0}
    for p in arguments
      point.x += p.x
      point.y += p.y
    point

  @subtractPoints: (p1, p2) -> # TODO: use this and addPoints everywhere
    {x: p1.x - p2.x, y: p1.y - p2.y}

  @cross: (p1, p2) ->
    p1.x * p2.y - p1.y * p2.x

  @getDistance: (p1, p2) ->
    Math.sqrt(Math.pow(p1.x - p2.x, 2) + Math.pow(p1.y - p2.y, 2))

  @getEdgeDistance: (gear1, gear2) ->
    axisDistance = @getDistance(gear1.location, gear2.location)
    Math.abs(axisDistance - gear1.pitchRadius - gear2.pitchRadius)

  @getDistanceToGear: (p, gear) ->
    #Math.max(0, @getDistance(p, gear.location) - gear.outerRadius)
    Math.max(0, @getDistance(p, gear.location) - gear.pitchRadius)

  @mod: (a, b) ->
    (a % b + b) % b

  @addAll: (set, elements) ->
    for element in elements
      set[element] = true
    set

  @makeSet: (elements...) ->
    set = {}
    @addAll(set, elements)

  @getPointOnSegment: (a, b, distance) ->
    fraction = distance / @getDistance(a, b)
    {x: a.x + fraction * (b.x - a.x), y: a.y + fraction * (b.y - a.y)}

  # find the point on the path at the given distance from its start
  @findPointOnPath: (path, distance) ->
    distanceToGo = distance
    i = 0
    numberOfPoints = path.length
    while distanceToGo > 0
      j = (i + 1) % numberOfPoints
      p1 = path[i]
      p2 = path[j]
      segmentLength = @getDistance(p1, p2)
      if distanceToGo <= segmentLength
        return @getPointOnSegment(p1, p2, distanceToGo)
      else
        i = j
        distanceToGo -= segmentLength
    return null

  @getLength: (path, isPathClosed = true) ->
    length = 0
    numberOfPoints = path.length
    finalIndex = numberOfPoints - (if isPathClosed then 0 else 1)
    for i in [0...finalIndex]
      j = (i + 1) % numberOfPoints
      length += @getDistance(path[i], path[j])
    length

  # http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
  @isPointInsidePolygon: (point, polygon) ->
    isPointInPolygon = false
    x = point.x
    y = point.y
    numberOfVertices = polygon.length
    j = numberOfVertices - 1
    for i in [0...numberOfVertices]
      pix = polygon[i].x
      piy = polygon[i].y
      pjx = polygon[j].x
      pjy = polygon[j].y
      if ((piy > y) isnt (pjy > y)) and (x < ((pjx - pix) * (y - piy) / (pjy - piy) + pix))
        isPointInPolygon = !isPointInPolygon
      j = i
    isPointInPolygon

  @isGearInsidePolygon: (gear, polygon) ->
    edgePointAtAngle = (angle) ->
      x: gear.location.x + Math.cos(angle) * gear.innerRadius
      y: gear.location.y + Math.sin(angle) * gear.innerRadius

    edgePoints = (edgePointAtAngle(0.25 * Math.PI * i) for i in [0...8])
    edgePoints.every((p) => @isPointInsidePolygon(p, polygon))

  @findGearsInsidePolygon: (polygon, gears) ->
    (gear for own id, gear of gears when @isGearInsidePolygon(gear, polygon))

  # find distance between point p and line segment ab
  # http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
  @getPointLineSegmentDistance: (p, a, b) ->
    segmentLength = @getDistance(a, b)
    if segmentLength is 0
      @getDistance(p, a)
    else
      t = ((p.x - a.x) * (b.x - a.x) + (p.y - a.y) * (b.y - a.y)) / (segmentLength * segmentLength)
      if t < 0
        @getDistance(p, a)
      else if t > 1
        @getDistance(p, b)
      else
        projection = {x: a.x + t * (b.x - a.x), y: a.y + t * (b.y - a.y)}
        @getDistance(p, projection)

  @doesGearIntersectSegment: (gear, a, b) ->
    #@getPointLineSegmentDistance(gear.location, a, b) < gear.outerRadius + @EPSILON
    @getPointLineSegmentDistance(gear.location, a, b) < gear.pitchRadius + @EPSILON

  @doesGearIntersectTriangle: (gear, triangle) ->
    @isPointInsidePolygon(gear.location, triangle) or
    @doesGearIntersectSegment(gear, triangle[0], triangle[1]) or
    @doesGearIntersectSegment(gear, triangle[0], triangle[2]) or
    @doesGearIntersectSegment(gear, triangle[1], triangle[2])

  @findGearsIntersectingTriangle: (gears, triangle) ->
    (gear for own id, gear of gears when @doesGearIntersectTriangle(gear, triangle))

  @findGearsIntersectingSegment: (gears, a, b) ->
    (gear for own id, gear of gears when @doesGearIntersectSegment(gear, a, b))

  @getPointPathDistance: (point, path, isPathClosed = true) ->
    # using points instead of segments
    distance = Number.MAX_VALUE
    numberOfPoints = path.length
    finalIndex = numberOfPoints - (if isPathClosed then 0 else 1)
    for i in [0...finalIndex]
      j = (i + 1) % numberOfPoints
      d = Math.max(0, @getPointLineSegmentDistance(point, path[i], path[j]))
      distance = Math.min(distance, d)
    distance

#  @getGearPathDistance: (gear, path, isPathClosed = true) ->
#    Math.max(0, @getPointPathDistance(gear.location, path, isPathClosed) - gear.pitchRadius)

  @doesChainCrossGear: (chain, gear) ->
    @getPointPathDistance(gear.location, chain.points) < gear.pitchRadius

  # return gear nearest to point a that intersects line segment ab or null if no such gear exists
  # if ignoredGears is specified, these gears will never be returned
  @findNearestIntersectingGear: (gears, a, b, ignoredGearIds = {}) ->
    intersectingGears = @findGearsIntersectingSegment(gears, a, b)
    intersectingGears.sort((g1, g2) => @getDistanceToGear(a, g1) - @getDistanceToGear(a, g2))
    for intersectingGear in intersectingGears
      unless (intersectingGear.id of ignoredGearIds)
        return intersectingGear
    null

  # http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
  @findLineSegmentIntersection: (segment1, segment2) ->
    p = segment1[0]
    r = @subtractPoints(segment1[1], p)
    q = segment2[0]
    s = @subtractPoints(segment2[1], q)
    crossRS = @cross(r, s)
    t = @cross(@subtractPoints(q, p), s) / crossRS
    u = @cross(@subtractPoints(q, p), r) / crossRS
    if Math.abs(crossRS) > @EPSILON and 0 <= t and t <= 1 and 0 <= u and u <= 1
      @addPoints(p, {x: t * r.x, y: t * r.y})
    else
      null

  # http://stackoverflow.com/questions/451426/how-do-i-calculate-the-surface-area-of-a-2d-polygon
  @findDirection: (polygon) ->
    numberOfPoints = polygon.length
    doubleArea = 0
    for i in [0...numberOfPoints]
      j = (i + 1) % numberOfPoints
      doubleArea += polygon[i].x * polygon[j].y
      doubleArea -= polygon[i].y * polygon[j].x
    if doubleArea > 0
      @Direction.CLOCKWISE
    else
      @Direction.COUNTER_CLOCKWISE

  # get side of point p relative to line ab
  # http://stackoverflow.com/questions/1560492/how-to-tell-wether-a-point-is-right-or-left-of-a-line
  @getSide: (p, a, b) ->
    side = (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x)
    if side is 0
      @Side.ON_LINE
    else if side < 0
      @Side.LEFT
    else
      @Side.RIGHT

  # get the two tangent points on a circle with center c and radius r from a given point p
  # tangent points are only valid if |pc| > r
  @findTangentPoints: (p, c, r) ->
    d = @getDistance(p, c)
    if Math.abs(d - r) < @EPSILON # p on circle
      [p, p]
    else
      l = Math.sqrt(d * d - r * r)
      alpha = Math.atan2(c.y - p.y, c.x - p.x)
      beta = Math.asin(r / d)
      p1x = p.x + Math.cos(alpha + beta) * l
      p1y = p.y + Math.sin(alpha + beta) * l
      p2x = p.x + Math.cos(alpha - beta) * l
      p2y = p.y + Math.sin(alpha - beta) * l
      tangentPoints = {}
      tangentPoints[@Side.RIGHT] = {x: p1x, y: p1y}
      tangentPoints[@Side.LEFT] = {x: p2x, y: p2y}
      tangentPoints

  @findGearTangentPoints: (p, gear) ->
    #@findTangentPoints(p, gear.location, gear.outerRadius)
    @findTangentPoints(p, gear.location, gear.pitchRadius)

  # http://en.wikipedia.org/wiki/Tangent_lines_to_circles
  @findExternalTangents: (centers, radii) ->
    largest = if radii[0] >= radii[1] then 0 else 1
    o1 = centers[largest]
    o2 = centers[1 - largest]
    r1 = radii[largest]
    r2 = radii[1 - largest]
    r3 = r1 - r2
    tangentPoints = @findTangentPoints(o2, o1, r3)
    angle = Math.atan2(o2.y - o1.y, o2.x - o1.x)
    offset1 = {x: Math.cos(angle + 0.5 * Math.PI) * r2, y: Math.sin(angle + 0.5 * Math.PI) * r2}
    offset2 = {x: Math.cos(angle - 0.5 * Math.PI) * r2, y: Math.sin(angle - 0.5 * Math.PI) * r2}
    tangentLine1 = [@addPoints(tangentPoints[@Side.LEFT], offset1), @addPoints(o2, offset1)]
    tangentLine2 = [@addPoints(tangentPoints[@Side.RIGHT], offset2), @addPoints(o2, offset2)]
    tangentLines = {}
    if o1 is centers[0]
      tangentLines[@Side.RIGHT] = tangentLine1
      tangentLines[@Side.LEFT] = tangentLine2
    else
      tangentLines[@Side.RIGHT] = [tangentLine2[1], tangentLine2[0]]
      tangentLines[@Side.LEFT] = [tangentLine1[1], tangentLine1[0]]
    tangentLines

  # http://en.wikipedia.org/wiki/Tangent_lines_to_circles
  @findInternalTangents: (centers, radii) ->
    largest = if radii[0] >= radii[1] then 0 else 1
    o1 = centers[largest]
    o2 = centers[1 - largest]
    r1 = radii[largest]
    r2 = radii[1 - largest]
    r3 = r1 + r2
    tangentPoints = @findTangentPoints(o2, o1, r3)
    ratio = r2 / r3
    tpl = tangentPoints[@Side.LEFT]
    tpr = tangentPoints[@Side.RIGHT]
    offset1 = {x: ratio * (o1.x - tpl.x), y: ratio * (o1.y - tpl.y)}
    offset2 = {x: ratio * (o1.x - tpr.x), y: ratio * (o1.y - tpr.y)}
    tangentLine1 = [@addPoints(tpl, offset1), @addPoints(o2, offset1)]
    tangentLine2 = [@addPoints(tpr, offset2), @addPoints(o2, offset2)]
    tangentLines = {}
    if o1 is centers[0]
      tangentLines[@Side.RIGHT] = tangentLine1
      tangentLines[@Side.LEFT] = tangentLine2
    else
      tangentLines[@Side.RIGHT] = [tangentLine1[1], tangentLine1[0]]
      tangentLines[@Side.LEFT] = [tangentLine2[1], tangentLine2[0]]
    tangentLines

  @findExternalTangentsOfGears: (gear1, gear2) ->
    #@findExternalTangents([gear1.location, gear2.location], [gear1.outerRadius, gear2.outerRadius])
    @findExternalTangents([gear1.location, gear2.location], [gear1.pitchRadius, gear2.pitchRadius])

  @findInternalTangentsOfGears: (gear1, gear2) ->
    #@findInternalTangents([gear1.location, gear2.location], [gear1.outerRadius, gear2.outerRadius])
    @findInternalTangents([gear1.location, gear2.location], [gear1.pitchRadius, gear2.pitchRadius])

  @findTangentLine: (gear1, gear2, innerGearIds, direction) ->
    gear1isInnerGear = (gear1.id in innerGearIds)
    if gear1isInnerGear is (direction is @Direction.CLOCKWISE)
      side = @Side.LEFT
    else
      side = @Side.RIGHT
    if gear1isInnerGear is (gear2.id in innerGearIds)
      @findExternalTangentsOfGears(gear1, gear2)[side]
    else
      @findInternalTangentsOfGears(gear1, gear2)[side]

  # TEMP
  @tempDrawLine: (a, b) ->
    canvas = document.getElementById("gearsketch_canvas")
    ctx = canvas.getContext("2d")
    ctx.save()
    ctx.lineWidth = 5
    ctx.strokeStyle = "red"
    ctx.beginPath()
    ctx.moveTo(a.x, a.y)
    ctx.lineTo(b.x, b.y)
    ctx.stroke()
    ctx.restore()

  @tempDrawCircle: (p) ->
    canvas = document.getElementById("gearsketch_canvas")
    ctx = canvas.getContext("2d")
    ctx.save()
    ctx.lineWidth = 5
    ctx.strokeStyle = "red"
    ctx.beginPath()
    ctx.arc(p.x, p.y, 30, 0, 2 * Math.PI, false)
    ctx.stroke()
    ctx.restore()
  # END TEMP


window.gearsketch.Util = Util

# requestAnimationFrame polyfill
# http://my.opera.com/emoller/blog/2011/12/20/requestanimationframe-for-smart-er-animating
do ->
  lastTime = 0
  vendors = ["ms", "moz", "webkit", "o"]
  for vendor in vendors when !window.requestAnimationFrame
    window.requestAnimationFrame = window[vendor + "RequestAnimationFrame"]
    window.cancelAnimationFrame = window[vendor + "CancelAnimationFrame"] or
    window[vendor + "CancelRequestAnimationFrame"]

  if !window.requestAnimationFrame
    window.requestAnimationFrame = (callback) ->
      currTime = new Date().getTime()
      timeToCall = Math.max(0, 16 - (currTime - lastTime))
      id = window.setTimeout((->
        callback(currTime + timeToCall)
      ), timeToCall)
      lastTime = currTime + timeToCall
      id

  if !window.cancelAnimationFrame
    window.cancelAnimationFrame = (id) ->
      clearTimeout(id)
