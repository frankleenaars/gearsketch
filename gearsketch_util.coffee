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
# ---------- Util -----------
# ---------------------------
# TODO: refactor line segment usage

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

  @getPointOnLineSegment: (a, b, distance) ->
    fraction = distance / a.distance(b)
    a.plus(b.minus(a).times(fraction))

  # find the point on the path at the given distance from its start
  @findPointOnPath: (path, distance) ->
    distanceToGo = distance
    i = 0
    numberOfPoints = path.length
    while distanceToGo > 0
      j = (i + 1) % numberOfPoints
      p1 = path[i]
      p2 = path[j]
      segmentLength = p1.distance(p2)
      if distanceToGo <= segmentLength
        return @getPointOnLineSegment(p1, p2, distanceToGo)
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

  # http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
  @findNearestPointOnLineSegment: (p, a, b) ->
    segmentLength = a.distance(b)
    if segmentLength is 0
      a
    else
      t = ((p.x - a.x) * (b.x - a.x) + (p.y - a.y) * (b.y - a.y)) / (segmentLength * segmentLength)
      if t < 0
        a
      else if t > 1
        b
      else
        a.plus(b.minus(a).times(t))

  @getPointLineSegmentDistance: (p, a, b) ->
    p.distance(@findNearestPointOnLineSegment(p, a, b))

  @doesGearIntersectSegment: (gear, a, b) ->
    @getPointLineSegmentDistance(gear.location, a, b) < (gear.pitchRadius + Util.EPSILON)

  # TODO: REMOVE?
  @doesGearIntersectTriangle: (gear, triangle) ->
    @isPointInsidePolygon(gear.location, triangle) or
    @doesGearIntersectSegment(gear, triangle[0], triangle[1]) or
    @doesGearIntersectSegment(gear, triangle[0], triangle[2]) or
    @doesGearIntersectSegment(gear, triangle[1], triangle[2])

  @findGearsIntersectingTriangle: (gears, triangle) ->
    (gear for own id, gear of gears when @doesGearIntersectTriangle(gear, triangle))
  # END REMOVE?

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
    r = segment1[1].minus(p)
    q = segment2[0]
    s = segment2[1].minus(q)
    crossRS = r.cross(s)
    t = q.minus(p).cross(s) / crossRS
    u = q.minus(p).cross(r) / crossRS
    if Math.abs(crossRS) > Util.EPSILON and 0 <= t and t <= 1 and 0 <= u and u <= 1
      p.plus(r.times(t))
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
    offset1 = o1.minus(tpl).times(ratio)
    offset2 = o1.minus(tpr).times(ratio)
    tangentLine1 = [tpl.plus(offset1), o2.plus(offset1)]
    tangentLine2 = [tpr.plus(offset2), o2.plus(offset2)]
    tangentLines = {}
    if o1 is centers[0]
      tangentLines[@Side.RIGHT] = tangentLine1
      tangentLines[@Side.LEFT] = tangentLine2
    else
      tangentLines[@Side.RIGHT] = [tangentLine1[1], tangentLine1[0]]
      tangentLines[@Side.LEFT] = [tangentLine2[1], tangentLine2[0]]
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
    nodes = turningObject for own id, turningObject of turningObjects
    if nodes.length < 2
      return []
    for i in [0...nodes.length - 1]
      for j in [(i + 1)...nodes.length]
        if nodes[i].connections(nodes[j].id)?
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
