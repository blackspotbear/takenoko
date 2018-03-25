const int MAX_MARCHING_STEPS = 255;
const float MIN_DIST = 0.0;
const float MAX_DIST = 100.0;
const float EPSILON = 0.0001;

// see: https://mobile.twitter.com/tompng/status/974333032495640576
float takenokoSDF(vec3 p) {
    const float e = 2.71828182846;

    float x2 = p.x * p.x;
    float y2 = p.y * p.y;

    float lhs = 1.0;
    lhs += 2.0 * p.z;
    lhs += pow(e, -p.z - 4.0);
    lhs += pow(e, -8.0 * p.z - 48.0);
    lhs += -8.0;
    lhs += sqrt(1.0 / 8.0 + x2 + y2 + 2.0 * (x2 + y2) * (x2 + y2));

    float rhs = 1.0;
    rhs *= 1.0 / (1.0 + pow(e, -16.0 * p.z - 72.0));
    rhs *= max(
        1.0 / (1.0 + pow(e, 8.0 * (p.z + p.x))) + 1.0 / (1.0 + pow(e, 8.0 * (p.z + p.x + 3.0))) + 1.0 / 2.0,
        1.0 / (1.0 + pow(e, 8.0 * (p.z - p.x))) + 1.0 / (1.0 + pow(e, 8.0 * (p.z - p.x + 3.0)))
    );

    return (lhs - rhs) * 0.024;
}

float sceneSDF(vec3 samplePoint) {
    return takenokoSDF(samplePoint);
}

float shortestDistanceToSurface(vec3 eye, vec3 marchingDirection, float start, float end) {
    float depth = start;
    for (int i = 0; i < MAX_MARCHING_STEPS; i++) {
        float dist = sceneSDF(eye + depth * marchingDirection);
        if (dist < EPSILON) {
			return depth;
        }
        depth += dist;
        if (depth >= end) {
            return end;
        }
    }
    return end;
}

vec3 rayDirection(float fieldOfView, vec2 size, vec2 fragCoord) {
    vec2 xy = fragCoord - size / 2.0;
    float z = size.y / tan(radians(fieldOfView) / 2.0);
    return normalize(vec3(xy, -z));
}

vec3 estimateNormal(vec3 p) {
    return normalize(vec3(
        sceneSDF(vec3(p.x + EPSILON, p.y, p.z)) - sceneSDF(vec3(p.x - EPSILON, p.y, p.z)),
        sceneSDF(vec3(p.x, p.y + EPSILON, p.z)) - sceneSDF(vec3(p.x, p.y - EPSILON, p.z)),
        sceneSDF(vec3(p.x, p.y, p.z  + EPSILON)) - sceneSDF(vec3(p.x, p.y, p.z - EPSILON))
    ));
}

vec3 phongContribForLight(vec3 k_d, vec3 k_s, float alpha, vec3 p, vec3 eye,
                          vec3 lightPos, vec3 lightIntensity) {
    vec3 N = estimateNormal(p);
    vec3 L = normalize(lightPos - p);
    vec3 V = normalize(eye - p);
    vec3 R = normalize(reflect(-L, N));

    float dotLN = dot(L, N);
    float dotRV = dot(R, V);

    if (dotLN < 0.0) {
        return vec3(0.0, 0.0, 0.0);
    }

    if (dotRV < 0.0) {
        return lightIntensity * (k_d * dotLN);
    }

    return lightIntensity * (k_d * dotLN + k_s * pow(dotRV, alpha));
}

vec3 phongIllumination(vec3 k_a, vec3 k_d, vec3 k_s, float alpha, vec3 p, vec3 eye) {
    const vec3 ambientLight = 0.5 * vec3(1.0, 1.0, 1.0);
    vec3 color = ambientLight * k_a;

    vec3 light1Pos = vec3(10.0 * sin(iTime),
                          5.0,
                          10.0 * cos(iTime));
    vec3 light1Intensity = vec3(0.4, 0.4, 0.4);
    color += phongContribForLight(k_d, k_s, alpha, p, eye,
                                  light1Pos,
                                  light1Intensity);

    vec3 light2Pos = vec3(10.0, 10.0, 10.0);
    vec3 light2Intensity = vec3(0.4, 0.4, 0.4);
    color += phongContribForLight(k_d, k_s, alpha, p, eye,
                                  light2Pos,
                                  light2Intensity);
    return color;
}

mat4 viewMatrix(vec3 eye, vec3 center, vec3 up) {
    vec3 f = normalize(center - eye);
    vec3 s = normalize(cross(f, up));
    vec3 u = cross(s, f);
    return mat4(
        vec4(s, 0.0),
        vec4(u, 0.0),
        vec4(-f, 0.0),
        vec4(0.0, 0.0, 0.0, 1)
    );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec3 viewDir = rayDirection(45.0, iResolution.xy, fragCoord);
    vec3 eye = vec3(0.0, 24.0, 0.0);


    mat4 viewToWorld = viewMatrix(eye, vec3(0.0, 0.0, 0.0), vec3(-1.0, 0.0, 1.0));


    vec3 worldDir = (viewToWorld * vec4(viewDir, 0.0)).xyz;

    float dist = shortestDistanceToSurface(eye, worldDir, MIN_DIST, MAX_DIST);

    if (dist > MAX_DIST - EPSILON) {
        fragColor = vec4(0.0, 0.4 + 0.2 * sin(iTime), 0.0, 0.0);
		return;
    }

    vec3 p = eye + dist * worldDir;

    vec3 K_a = vec3(0.5, 0.25, 0.25);
    vec3 K_d = vec3(0.7, 0.4, 0.4);
    vec3 K_s = vec3(1.0, 0.8, 0.8);
    float shininess = 4.0;

    vec3 color = phongIllumination(K_a, K_d, K_s, shininess, p, eye);

    fragColor = vec4(color, 1.0);
}