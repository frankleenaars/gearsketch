# By Frank Leenaars
# University of Twente - Department of Instructional Technology
"use strict"

window.gearsketch = {}

# ---------------------------
# ---------- Point ----------
# ---------------------------
class Point
  constructor: (@x, @y) ->

  plus: (p) ->
    new Point(@x + p.x, @y + p.y)

  minus: (p) ->
    new Point(@x - p.x, @y - p.y)

  times: (n) ->
    new Point(n * @x, n * @y)

  distance: (p) ->
    Math.sqrt(Math.pow(@x - p.x, 2) + Math.pow(@y - p.y, 2))

  cross: (p) ->
    @x * p.y - @y * p.x

  clone: ->
    new Point(@x, @y)

  @polar: (theta, r) ->
    new Point(r * Math.cos(theta), r * Math.sin(theta))

window.gearsketch.Point = Point

# ---------------------------
# ------- ArcSegment --------
# ---------------------------
# TODO: for consistency with LineSegment @start & @end should be points, old ones called @startAngle, @endAngle
class ArcSegment
  constructor: (@center, @radius, @start, @end, @direction) ->

  getLength: ->
    angle = if @direction is Util.Direction.CLOCKWISE
      Util.mod(@end - @start, 2 * Math.PI)
    else
      Util.mod(@start - @end, 2 * Math.PI)
    angle * @radius

  findPoint: (distanceToGo) ->
    angleToGo = distanceToGo / @radius
    angle = @start + (if @direction is Util.Direction.CLOCKWISE then angleToGo else -angleToGo)
    @center.plus(Point.polar(angle, @radius))

  pointOnCircle: (angle) ->
    @center.plus(Point.polar(angle, @radius))

  startPoint: ->
    @pointOnCircle(@start)

  endPoint: ->
    @pointOnCircle(@end)

  doesArcContainAngle: (angle) ->
    if @direction is Util.Direction.CLOCKWISE
      Util.mod(@end - @start, 2 * Math.PI) > Util.mod(angle - @start, 2 * Math.PI)
    else
      Util.mod(@start - @end, 2 * Math.PI) > Util.mod(@start - angle, 2 * Math.PI)

  getDistanceToPoint: (point) ->
    angle = Math.atan2(point.y - @center.y, point.x - @center.x)
    if @doesArcContainAngle(angle)
      Math.abs(point.distance(@center) - @radius)
    else
      startPoint = @center.plus(Point.polar(@start, @radius))
      endPoint = @center.plus(Point.polar(@end, @radius))
      Math.min(point.distance(startPoint), point.distance(endPoint))

  intersectsLineSegment: (lineSegment) ->
    # check if circle intersects line
    # http://mathworld.wolfram.com/Circle-LineIntersection.html
    p1 = lineSegment.start.minus(@center)
    p2 = lineSegment.end.minus(@center)
    dx = p2.x - p1.x
    dy = p2.y - p1.y
    dr = Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
    if dr is 0
      return false
    d = p1.x * p2.y - p2.x * p1.y
    discriminant = Math.pow(@radius, 2) * Math.pow(dr, 2) - Math.pow(d, 2)
    if discriminant < 0
      false
    else
      i1x = (d * dy + Util.sign(dy) * dx * Math.sqrt(discriminant)) / Math.pow(dr, 2)
      i1y = (-d * dx + Math.abs(dy) * Math.sqrt(discriminant)) / Math.pow(dr, 2)
      i1 = new Point(i1x, i1y).plus(@center)
      if lineSegment.getDistanceToPoint(i1) < Util.EPSILON and @getDistanceToPoint(i1) < Util.EPSILON
        return true
      i2x = (d * dy - Util.sign(dy) * dx * Math.sqrt(discriminant)) / Math.pow(dr, 2)
      i2y = (-d * dx - Math.abs(dy) * Math.sqrt(discriminant)) / Math.pow(dr, 2)
      i2 = new Point(i2x, i2y).plus(@center)
      if lineSegment.getDistanceToPoint(i2) < Util.EPSILON and @getDistanceToPoint(i2) < Util.EPSILON
        return true
      false

  # TODO: fix algorithms
  getDistanceToSegment: (segment) ->
    if segment instanceof ArcSegment
      if @center.distance(segment.center) > Util.EPSILON
        angle1 = Math.atan2(segment.center.y - @center.y, segment.center.x - @center.x)
        angle2 = Util.mod(angle1 + Math.PI, 2 * Math.PI)
        if @doesArcContainAngle(angle1) and segment.doesArcContainAngle(angle2)
          centerDistance = @center.distance(segment.center)
          return Math.max(0, centerDistance - @radius - segment.radius)
      Math.min(@getDistanceToPoint(segment.startPoint())
      , @getDistanceToPoint(segment.endPoint())
      , segment.getDistanceToPoint(@startPoint())
      , segment.getDistanceToPoint(@endPoint()))
    else # segment is LineSegment
      if @intersectsLineSegment(segment)
        0
      else
        pointNearestToCenter = segment.findNearestPoint(@center)
        Math.min(@getDistanceToPoint(pointNearestToCenter)
        , @getDistanceToPoint(segment.start)
        , @getDistanceToPoint(segment.end)
        , segment.getDistanceToPoint(@startPoint())
        , segment.getDistanceToPoint(@endPoint()))


  clone: ->
    new ArcSegment(@center.clone(), @radius, @start, @end, @direction)

window.gearsketch.ArcSegment = ArcSegment

# ---------------------------
# ------- LineSegment -------
# ---------------------------
class LineSegment
  constructor: (@start, @end) ->

  getLength: ->
    @start.distance(@end)

  findPoint: (distanceToGo) ->
    fraction = distanceToGo / @start.distance(@end)
    @start.plus(@end.minus(@start).times(fraction))

  # http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
  findNearestPoint: (p) ->
    segmentLength = @getLength()
    if segmentLength is 0
      @start
    else
      t = ((p.x - @start.x) * (@end.x - @start.x) + (p.y - @start.y) * (@end.y - @start.y)) / Math.pow(segmentLength, 2)
      if t < 0
        @start
      else if t > 1
        @end
      else
        @start.plus(@end.minus(@start).times(t))

  getDistanceToPoint: (point) ->
    point.distance(@findNearestPoint(point))

  # http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
  findIntersection: (lineSegment) ->
    p = @start
    r = @end.minus(p)
    q = lineSegment.start
    s = lineSegment.end.minus(q)
    crossRS = r.cross(s)
    t = q.minus(p).cross(s) / crossRS
    u = q.minus(p).cross(r) / crossRS
    if Math.abs(crossRS) > Util.EPSILON and 0 <= t and t <= 1 and 0 <= u and u <= 1
      p.plus(r.times(t))
    else
      null

  getDistanceToSegment: (segment) ->
    if segment instanceof LineSegment
      if @findIntersection(segment)
        0
      else
        Math.min(
          @getDistanceToPoint(segment.start)
        , @getDistanceToPoint(segment.end)
        , segment.getDistanceToPoint(@start)
        , segment.getDistanceToPoint(@end))
    else # segment is ArcSegment
      segment.getDistanceToSegment(this)

  clone: ->
    new LineSegment(@start.clone(), @end.clone())

window.gearsketch.LineSegment = LineSegment

# ---------------------------
# ---------- Util -----------
# ---------------------------
class Util
  # imports
  Point = window.gearsketch.Point

  # -- constants --
  @MODULE: 6
  @AXIS_RADIUS: 1.5 * @MODULE
  @MIN_GEAR_TEETH: 8
  @MIN_STACKED_GEARS_TEETH_DIFFERENCE: 4
  @SNAPPING_DISTANCE: 2 * @MODULE
  @EPSILON: 0.000001

  # -- enums --
  @Direction:
    CLOCKWISE: "clockwise"
    COUNTER_CLOCKWISE: "counterclockwise"

  @Side:
    LEFT: "left"
    RIGHT: "right"

  # http://stackoverflow.com/questions/728360/most-elegant-way-to-clone-a-javascript-object
  @clone: (obj) ->
    if !obj? or (typeof obj isnt "object")
      return obj

    knownClasses = ["Point", "Gear", "ArcSegment", "LineSegment", "Chain"]
    if obj.constructor.name in knownClasses
      return obj.clone()

    if obj instanceof Array
      copy = []
      for i in [0...obj.length]
        copy[i] = @clone(obj[i])
      return copy

    if obj instanceof Object
      copy = {}
      for own key, val of obj
        copy[key] = @clone(obj[key])
      return copy

    throw new Error("Unable to clone object. Its type is not supported.")

  @getEdgeDistance: (gear1, gear2) ->
    axisDistance = gear1.location.distance(gear2.location)
    Math.abs(axisDistance - gear1.pitchRadius - gear2.pitchRadius)

  @getDistanceToGear: (p, gear) ->
    Math.max(0, p.distance(gear.location) - gear.pitchRadius)

  @mod: (a, b) ->
    (a % b + b) % b

  @sign: (x) ->
    if x < 0 then -1 else 1

  @addAll: (set, elements) ->
    for element in elements
      set[element] = true
    set

  # compares two arrays of numbers
  @areArraysEqual: (a1, a2, orderMatters = true) ->
    if !a1? or !a2?
      false
    else if a1.length isnt a2.length
      false
    else if orderMatters
      a1.every((e, i) -> e is a2[i])
    else # order doesn't matter
      sa1 = (e for e in a1).sort((e1, e2) -> e1 - e2)
      sa2 = (e for e in a2).sort((e1, e2) -> e1 - e2)
      @areArraysEqual(sa1, sa2)

  @makeSet: (elements...) ->
    set = {}
    @addAll(set, elements)

  # find the point on the path at the given distance from its start
  @findPointOnPath: (path, distance) ->
    distanceToGo = distance
    i = 0
    numberOfPoints = path.length
    while distanceToGo > 0
      j = (i + 1) % numberOfPoints
      segment = new LineSegment(path[i], path[j])
      segmentLength = segment.getLength()
      if distanceToGo <= segmentLength
        return segment.findPoint(distanceToGo)
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
      length += path[i].distance(path[j])
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
      gear.location.plus(Point.polar(angle, gear.innerRadius))
    edgePoints = (edgePointAtAngle(0.25 * Math.PI * i) for i in [0...8])
    edgePoints.every((p) => @isPointInsidePolygon(p, polygon))

  @findGearsInsidePolygon: (polygon, gears) ->
    (gear for own id, gear of gears when @isGearInsidePolygon(gear, polygon))

  @doesGearIntersectLineSegment: (gear, segment) ->
    segment.getDistanceToPoint(gear.location) < (gear.pitchRadius + Util.EPSILON)

  @findGearsIntersectingSegment: (gears, segment) ->
    (gear for own id, gear of gears when @doesGearIntersectLineSegment(gear, segment))

  @getPointPathDistance: (point, path, isPathClosed = true) ->
    # using points instead of segments
    distance = Number.MAX_VALUE
    numberOfPoints = path.length
    finalIndex = numberOfPoints - (if isPathClosed then 0 else 1)
    for i in [0...finalIndex]
      j = (i + 1) % numberOfPoints
      segment = new LineSegment(path[i], path[j])
      d = Math.max(0, segment.getDistanceToPoint(point))
      distance = Math.min(distance, d)
    distance

  # return gear nearest to lineSegment.start that intersects lineSegment or null if no such gear exists
  # if ignoredGears is specified, these gears will never be returned
  @findNearestIntersectingGear: (gears, lineSegment, ignoredGearIds = {}) ->
    intersectingGears = @findGearsIntersectingSegment(gears, lineSegment)
    intersectingGears.sort((g1, g2) =>
      @getDistanceToGear(lineSegment.start, g1) - @getDistanceToGear(lineSegment.start, g2))
    for intersectingGear in intersectingGears
      unless intersectingGear.id of ignoredGearIds
        return intersectingGear
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

  # get the two tangent points on a circle with center c and radius r from a given point p
  # tangent points are only valid if |pc| > r
  @findTangentPoints: (p, c, r) ->
    tangentPoints = {}
    d = p.distance(c)
    if Math.abs(d - r) < Util.EPSILON # p on circle
      tangentPoints[@Side.RIGHT] = p.clone()
      tangentPoints[@Side.LEFT] = p.clone()
    else
      l = Math.sqrt(d * d - r * r)
      alpha = Math.atan2(c.y - p.y, c.x - p.x)
      beta = Math.asin(r / d)
      tangentPoints[@Side.RIGHT] = p.plus(Point.polar(alpha + beta, l))
      tangentPoints[@Side.LEFT] = p.plus(Point.polar(alpha - beta, l))
    tangentPoints

  @findGearTangentPoints: (p, gear) ->
    @findTangentPoints(p, gear.location, gear.pitchRadius)

  # http://en.wikipedia.org/wiki/Tangent_lines_to_circles
  @findExternalTangents: (centers, radii) ->
    largest = if radii[0] >= radii[1] then 0 else 1
    o1 = centers[largest]
    o2 = centers[1 - largest]
    r1 = radii[largest]
    r2 = radii[1 - largest]
    r3 = r1 - r2
    if r3 is 0
      tangentPoints = {}
      tangentPoints[@Side.LEFT] = o1
      tangentPoints[@Side.RIGHT] = o1
      angle = Math.atan2(o2.y - o1.y, o2.x - o1.x)
      offset1 = Point.polar(angle + 0.5 * Math.PI, r1)
      offset2 = Point.polar(angle - 0.5 * Math.PI, r1)
    else
      tangentPoints = @findTangentPoints(o2, o1, r3)
      ratio = r2 / r3
      tpl = tangentPoints[@Side.LEFT]
      tpr = tangentPoints[@Side.RIGHT]
      offset1 = tpl.minus(o1).times(ratio)
      offset2 = tpr.minus(o1).times(ratio)
    tangentLine1 = [tangentPoints[@Side.LEFT].plus(offset1), o2.plus(offset1)]
    tangentLine2 = [tangentPoints[@Side.RIGHT].plus(offset2), o2.plus(offset2)]
    tangentLines = {}
    if o1 is centers[0]
      tangentLines[@Side.RIGHT] = new LineSegment(tangentLine1[0], tangentLine1[1])
      tangentLines[@Side.LEFT] = new LineSegment(tangentLine2[0], tangentLine2[1])
    else
      tangentLines[@Side.RIGHT] = new LineSegment(tangentLine2[1], tangentLine2[0])
      tangentLines[@Side.LEFT] = new LineSegment(tangentLine1[1], tangentLine1[0])
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
    offset1 = o1.minus(tpl).times(ratio)
    offset2 = o1.minus(tpr).times(ratio)
    tangentLine1 = [tpl.plus(offset1), o2.plus(offset1)]
    tangentLine2 = [tpr.plus(offset2), o2.plus(offset2)]
    tangentLines = {}
    if o1 is centers[0]
      tangentLines[@Side.RIGHT] = new LineSegment(tangentLine1[0], tangentLine1[1])
      tangentLines[@Side.LEFT] = new LineSegment(tangentLine2[0], tangentLine2[1])
    else
      tangentLines[@Side.RIGHT] = new LineSegment(tangentLine1[1], tangentLine1[0])
      tangentLines[@Side.LEFT] = new LineSegment(tangentLine2[1], tangentLine2[0])
    tangentLines

  @findExternalTangentsOfGears: (gear1, gear2) ->
    @findExternalTangents([gear1.location, gear2.location], [gear1.pitchRadius, gear2.pitchRadius])

  @findInternalTangentsOfGears: (gear1, gear2) ->
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

  @findAllSimplePathsForNodes: (turningObjects, goalNode, nodesVisited) ->
    paths = []
    currentNode = nodesVisited[nodesVisited.length - 1]
    for own neighborId of currentNode.connections
      neighbor = turningObjects[neighborId]
      unless neighbor in nodesVisited
        nv = nodesVisited.slice(0)
        nv.push(neighbor)
        if neighbor is goalNode
          paths.push(nv)
        else
          paths = paths.concat(@findAllSimplePathsForNodes(turningObjects, goalNode, nv))
    return paths

  @findAllSimplePathsBetweenNeighbors: (turningObjects) ->
    paths = []
    nodes = (turningObject for own id, turningObject of turningObjects)
    if nodes.length < 2
      return []
    for i in [0...nodes.length - 1]
      for j in [(i + 1)...nodes.length]
        if nodes[i].connections[nodes[j].id]?
          paths = paths.concat(@findAllSimplePathsForNodes(turningObjects, nodes[j], [nodes[i]]))
    for i in [0...paths.length]
      paths.push(paths[i].slice(0).reverse())
    paths

  # TEMP
  @tempRegisterDrawMethod: (object, drawFunction) ->
    @tempRedraw = ->
      drawFunction.call(object)

  @tempDrawPath: (path, isPathClosed = false, shouldRedraw = true) ->
    if shouldRedraw
      @tempRedraw()
    canvas = document.getElementById("gearsketch_canvas")
    ctx = canvas.getContext("2d")
    ctx.save()
    ctx.lineWidth = 5
    ctx.strokeStyle = "red"
    ctx.beginPath()
    numberOfPoints = path.length
    lastPointIndex = path.length - (if isPathClosed then 0 else 1)
    for i in [0...lastPointIndex]
      j = (i + 1) % numberOfPoints
      ctx.moveTo(path[i].x, path[i].y)
      ctx.lineTo(path[j].x, path[j].y)
    ctx.stroke()
    ctx.restore()

  @tempDrawLine: (a, b, shouldRedraw = true) ->
    if shouldRedraw
      @tempRedraw()
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

  @tempDrawCircle: (p, radius = 30, shouldRedraw = true) ->
    if shouldRedraw
      @tempRedraw()
    canvas = document.getElementById("gearsketch_canvas")
    ctx = canvas.getContext("2d")
    ctx.save()
    ctx.lineWidth = 2
    ctx.strokeStyle = "red"
    ctx.beginPath()
    ctx.arc(p.x, p.y, radius, 0, 2 * Math.PI, false)
    ctx.stroke()
    ctx.restore()

  @tempDrawPoint: (p, shouldRedraw = true) ->
    if shouldRedraw
      @tempRedraw()
    canvas = document.getElementById("gearsketch_canvas")
    ctx = canvas.getContext("2d")
    ctx.save()
    ctx.fillStyle = "red"
    ctx.beginPath()
    ctx.arc(p.x, p.y, 5, 0, 2 * Math.PI, false)
    ctx.fill()
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

# Function.name support for IE
# http://matt.scharley.me/2012/03/09/monkey-patch-name-ie.html
do ->
  if !Function.prototype.name? and Object.defineProperty?
    Object.defineProperty(Function.prototype, 'name',
      get: ->
        funcNameRegex = /function\s([^(]{1,})\(/
        results = (funcNameRegex).exec((this).toString())
        if (results? and results.length > 1) then results[1].trim() else ""
      set: (value) ->
    )
