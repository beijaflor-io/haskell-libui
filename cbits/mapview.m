// 15 august 2015
#import "uipriv_darwin.h"
#import "MapKit/MapKit.h"

struct uiMapview {
  uiDarwinControl c;

  MKMapView *mapview;
};

@interface mapviewDelegateClass : NSObject {
  struct mapTable *maps;
}
- (void)registerMap:(uiMapview *)m;
@end

@implementation mapviewDelegateClass
- (id)init {
  self = [super init];
  if (self)
    self->maps = newMap();
  return self;
}
- (void)dealloc {
  mapDestroy(self->maps);
  [super dealloc];
}

- (void)registerMap:(uiMapview *)m {
  mapSet(self->maps, m->mapview, m);
  m->mapview.delegate = self;
}
@end

static mapviewDelegateClass *mapDelegate = nil;

uiDarwinControlAllDefaultsExceptDestroy(uiMapview, mapview)

static void uiMapviewDestroy(uiControl *c) {
  uiMapview* m = uiMapview(c);
  [m->mapview release];
  uiFreeControl(c);
}

void uiMapviewSetRegion(uiMapview *m) {
  NSLog(@"Setting region");

  CLLocationCoordinate2D coord;
  coord.latitude = 37.423617;
  coord.longitude = -122.220154;
  NSLog(@"Created coord");

  MKCoordinateSpan span;
  span.latitudeDelta = 10;
  span.longitudeDelta = 10;
  NSLog(@"Created span");

  MKCoordinateRegion region = {coord, span};
  NSLog(@"Created region");

  MKCoordinateRegion aregion = [m->mapview regionThatFits:region];
  NSLog(@"Adjusted region");
  [m->mapview setRegion:aregion animated:YES];
  NSLog(@"Region set");
}

uiMapview *uiNewMapview() {
  uiMapview *m;
  uiDarwinNewControl(uiMapview, m);

  m->mapview = [[MKMapView alloc] initWithFrame:NSZeroRect];
  m->mapview.mapType = MKMapTypeStandard;

  if (mapDelegate == nil) {
    mapDelegate = [[mapviewDelegateClass new] autorelease];
    [delegates addObject:mapDelegate];
  }
  [mapDelegate registerMap:m];

  return m;
}
