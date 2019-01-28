import numpy as np
import cv2
import math
import sys

w = 600
h = 400


class Vec3f:
    def __init__(self, x:float = 0, y:float = 0, z:float = 0):
        self.v = np.array([x, y, z], float) 
    def norm(self):
        return math.sqrt(np.dot(self.v, self.v))        
    def normalize(self):
        self.v = self.v * (1.0 / self.norm())
        return self
    def __sub__(self, other):
        return Vec3f(self.v[0] - other.v[0], self.v[1]-other.v[1], self.v[2]-other.v[2])
    def __add__(self, other):
        return Vec3f(self.v[0] + other.v[0], self.v[1] + other.v[1], self.v[2] + other.v[2])

    def __mul__(self, n):
        return Vec3f(self.v[0] * n, self.v[1] * n, self.v[2] * n)

    def x(self):
        return self.v[0]
    def y(self):
        return self.v[1]
    def z(self):
        return self.v[2]

class Vec4f:
    def __init__(self, x, y, z, a):
        self.v = np.array([x, y, z, a], float) 

def dot(v1:Vec3f, v2: Vec3f):
    return np.dot(v1.v, v2.v)

class Light:
    def __init__(self, p: Vec3f, i: float):
        self.position = p
        self.intensity = i

class Material:
    def __init__(self, r:float = 1, a: Vec4f = Vec4f(1,0,0,0), color: Vec3f = Vec3f(), spec: float = 0):
        self.refractive_index = r
        self.albedo = a
        self.diffuse_color = color
        self.specular_exponent = spec
    

class Sphere:
    def __init__(self, c: Vec3f, r: float, mat:Material):
        self.center = c
        self.radius = r
        self.material = mat

    def ray_intersect(self, orig: Vec3f, direct: Vec3f):
        t0 = sys.float_info.max
        L = self.center - orig
        tca = dot(L, direct)
        d2 = dot(L, L) - tca * tca
        if (d2 > self.radius * self.radius): return (False, 0)
        thc = math.sqrt(self.radius * self.radius - d2)
        t0 = tca - thc
        t1 = tca + thc
        if (t0 < 0): t0 = t1
        if (t0 < 0): return (False, 0)
        return (True, t0)

def reflect(I:Vec3f, N:Vec3f):
    return I - N * 2 * dot(I, N)

def refract(I:Vec3f, N:Vec3f, eta_t:float, eta_i:float =1.):
    cosi = - max(-1., min(1., dot(I,N)))
    if cosi < 0: return refract(I, Vec3f(0,0,0)-N, eta_i, eta_t)
    eta = eta_i / eta_t
    k = 1 - eta * eta *(1 - cosi * cosi)
    if k < 0:
        return Vec3f(1,0,0)
    return I * eta + N * (eta * cosi - math.sqrt(k))


ivory = Material(1.0, Vec4f(0.6,  0.3, 0.1, 0.0), Vec3f(0.4, 0.4, 0.3),   50.)
glass = Material(1.5, Vec4f(0.1,  0.5, 0.1, 0.8), Vec3f(0.6, 0.7, 0.4),  225.)
red_rubber = Material (1.0, Vec4f(0.9,  0.1, 0.0, 0.0), Vec3f(0.1, 0.1, 0.3),   10.);
mirror = Material(1.0, Vec4f(0.0, 10.0, 0.8, 0.0), Vec3f(1.0, 1.0, 1.0), 1425.)

asp = [ \
        Sphere(Vec3f(-2,    0,   -16), 3, ivory),   \
        Sphere(Vec3f(-1.0, -1.5, -12), 2, glass),   \
        Sphere(Vec3f( 1.5, -0.5, -18), 3, red_rubber),  \
        Sphere(Vec3f( 7,    5,   -18), 4,     mirror)]
lights = [Light(Vec3f(-20, 20, 20), 1.5), Light(Vec3f( 30, 50, -25), 1.8), Light(Vec3f( 30, 20,  30), 1.7)]

# ..............................................................................................

def scene_intersect(orig:Vec3f, direct:Vec3f, spheres):

    spheres_dist = sys.float_info.max
    hit = Vec3f()
    N = Vec3f()
    mat = Material()
    for S in spheres:
        r = S.ray_intersect(orig, direct)
        dist_i = r[1]
        if r[0] and (dist_i < spheres_dist):
            spheres_dist = dist_i
            hit = orig + direct * dist_i
            N = (hit - S.center).normalize()
            mat = S.material

    checkerboard_dist = sys.float_info.max
    if (abs(direct.y()) > 0.001):
        d = -(orig.y() + 4) / direct.y()
        pt = orig + direct * d
        if d > 0 and abs(pt.x()) < 10 and (pt.z() < -10) and (pt.z() > -30) and (d < spheres_dist):
            checkerboard_dist = d
            hit = pt;
            N = Vec3f(0,1,0);
            if ((int(.5 * hit.x()) + 1000) + int(.5 * hit.z()) & 1) > 0:
                mat.diffuse_color = Vec3f(.3, .3, .3)
            else:
                mat.diffuse_color = Vec3f(.1, .2, .1);


    return (min(spheres_dist, checkerboard_dist) < 1000, hit, N, mat)

# .....................................................................................

def cast_ray(orig: Vec3f, direct: Vec3f, spheres, lights, depth = 0):

    (b, point, N, material) = scene_intersect(orig, direct, spheres)

    if (depth > 4) or (not b):
        return Vec3f(0.5, 0.4, 0.1)
    

    reflect_dir = reflect(direct, N).normalize()
    refract_dir = refract(direct, N, material.refractive_index).normalize()

    if dot(reflect_dir, N) < 0:
        reflect_orig = point - N * 0.001
    else:
        reflect_orig = point + N * 0.001

    if dot(refract_dir, N) < 0:
        refract_orig = point - N * 0.001
    else:
        refract_orig = point + N * 0.001

    reflect_color = cast_ray(reflect_orig, reflect_dir, spheres, lights, depth + 1);
    refract_color = cast_ray(refract_orig, refract_dir, spheres, lights, depth + 1);

    diffuse_light_intensity = 0
    specular_light_intensity = 0

    for L in lights:
        light_dir      = (L.position - point).normalize();
        light_distance = (L.position - point).norm();
        if dot(light_dir, N) < 0:
            shadow_orig =  point - N * 0.001
        else:
            shadow_orig =  point + N * 0.001
        (b,  shadow_pt, shadow_N, tmpmaterial) = scene_intersect(shadow_orig, light_dir, spheres)
        if b and ((shadow_pt-shadow_orig).norm() < light_distance): continue

        diffuse_light_intensity  = diffuse_light_intensity + L.intensity * max(0., dot(light_dir,N))
        specular_light_intensity = specular_light_intensity + \
            math.pow(max(0., -dot(reflect(light_dir * -1, direct), N) ), material.specular_exponent) * L.intensity

    return material.diffuse_color * diffuse_light_intensity * material.albedo.v[0] + \
            Vec3f(1., 1., 1.) * specular_light_intensity * material.albedo.v[1] + \
            reflect_color * material.albedo.v[2] + refract_color * material.albedo.v[3]

# ......................................................................................

def render(asp, lights):
    z = np.zeros((h, w, 3), dtype = float)
    fov = 1.0

    for j in range(h):
        for i in range(w):
            x =  (2*(i + 0.5)/w  - 1) * math.tan(fov/2.) * w/h
            y = -(2*(j + 0.5)/h - 1) * math.tan(fov/2.)
            direct = Vec3f(x, y, -1).normalize()
            v = cast_ray(Vec3f(0,0,0), direct, asp, lights)
            z[j, i] = v.v
        print(j)

    cv2.imshow('img', z)
    cv2.waitKey(0)
    cv2.destroyAllWindows()
    cv2.imwrite('render01.jpg', z * 255)

render(asp, lights)

