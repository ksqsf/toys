#import <Cocoa/Cocoa.h>
#import <QuartzCore/QuartzCore.h>

@interface CursorLayer : CALayer

@property (nonatomic) CGFloat cursorWidth;
@property (nonatomic) CGFloat cursorHeight;
@property (nonatomic, strong) NSColor *cursorColor;

- (void)moveToX:(CGFloat)x Y:(CGFloat)y;

@end


@implementation CursorLayer

- (instancetype)init
{
  self = [super init];
  if (self) {
    self.cursorWidth = 2.0f;
    self.cursorHeight = 18.0f;
    self.cursorColor = [NSColor redColor];
    [self setupCursor];
  }
  return self;
}

- (void)setupCursor
{
  self.backgroundColor = self.cursorColor.CGColor;
  self.cornerRadius = 2.0;
  self.frame = CGRectMake(0, 0, 10, 20);
  // Add text-overlapping behavior if needed
  // You might have to set a delegate for the layer to customize rendering with a method like `drawLayer:inContext:`
}

- (void)moveToX:(CGFloat)x Y:(CGFloat)y
{
  CABasicAnimation *moveAnimation = [CABasicAnimation animationWithKeyPath:@"position"];
  moveAnimation.fromValue = [NSValue valueWithPoint:self.position];
  moveAnimation.toValue = [NSValue valueWithPoint:NSMakePoint(x + self.cursorWidth / 2, y + self.cursorHeight / 2)];
  moveAnimation.duration = 5.5; // Animation duration
  moveAnimation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionEaseInEaseOut];
  self.position = CGPointMake(x + self.cursorWidth / 2, y + self.cursorHeight / 2);
  [self addAnimation:moveAnimation forKey:@"moveCursor"];
}

@end

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property(strong) CursorLayer *cursorLayer;
@property(strong) NSWindow *window;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)notification
{
  NSRect windowRect = NSMakeRect(100, 100, 500, 300);
  NSWindow *window = [[NSWindow alloc] initWithContentRect:windowRect
                                                 styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskResizable
                                                   backing:NSBackingStoreBuffered
                                                     defer:NO];
  self.window = window;
  [window setTitle:@"Animation"];

  NSVisualEffectView *bgView = [[NSVisualEffectView alloc] initWithFrame:windowRect];
  window.contentView = bgView;
  bgView.wantsLayer = YES;

  CursorLayer *cursorLayer = [CursorLayer layer];
  [bgView.layer addSublayer:cursorLayer];

  NSTrackingArea *trackingArea = [[NSTrackingArea alloc] initWithRect:bgView.bounds
                                                              options:NSTrackingMouseMoved | NSTrackingActiveInKeyWindow
                                                                owner:self
                                                             userInfo:nil];
  [bgView addTrackingArea:trackingArea];

  [window makeKeyAndOrderFront:nil];

  self.cursorLayer = cursorLayer;
}

- (void)animateLayerToPoint:(NSPoint)point {
  CGPoint startPosition = self.cursorLayer.position;
  CGFloat dx = point.x - startPosition.x;
  CGFloat dy = point.y - startPosition.y;
  CGFloat distance = hypot(dx, dy);

  // Animation 1: Move
  CABasicAnimation *positionAnimation = [CABasicAnimation animationWithKeyPath:@"position"];
  positionAnimation.fromValue = [NSValue valueWithPoint:NSPointFromCGPoint(self.cursorLayer.presentationLayer.position)];
  positionAnimation.toValue = [NSValue valueWithPoint:point];
  positionAnimation.timingFunction = [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionDefault];
  //[self.cursorLayer addAnimation:positionAnimation forKey:@"positionAnimation"];

  // Animation 2: Skew
  CGFloat skewX = 0.3f;
  CATransform3D skewedTransform = CATransform3DIdentity;
  skewedTransform.m11 = fabs(dx);
  skewedTransform.m22 = fabs(dy);
  skewedTransform.m21 = skewX;
  CAKeyframeAnimation *morphAnimation = [CAKeyframeAnimation animationWithKeyPath:@"transform"];
  morphAnimation.values = @[
    [NSValue valueWithCATransform3D:CATransform3DIdentity],
    [NSValue valueWithCATransform3D:skewedTransform],
    [NSValue valueWithCATransform3D:CATransform3DIdentity]
  ];
  morphAnimation.keyTimes = @[@0.0, @0.5 , @1.0];
  morphAnimation.timingFunctions = @[
    [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionLinear],
    [CAMediaTimingFunction functionWithName:kCAMediaTimingFunctionLinear]
  ];


  CAAnimationGroup *group = [CAAnimationGroup animation];
  group.animations = @[positionAnimation, morphAnimation];
  group.duration = 0.25;
  group.fillMode = kCAFillModeForwards;
  group.removedOnCompletion = NO;

  [self.cursorLayer addAnimation:group forKey:@"moveAnimation"];

  self.cursorLayer.position = CGPointMake(point.x, point.y);
}

- (void)mouseMoved:(NSEvent *)event {
    NSPoint pointInView = [self.window.contentView convertPoint:event.locationInWindow fromView:nil];
    [self animateLayerToPoint:pointInView];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)sender {
    return YES;
}

@end

int main()
{
  @autoreleasepool {
    NSApplication *application = [NSApplication sharedApplication];
    AppDelegate *delegate = [[AppDelegate alloc] init];
    application.delegate = delegate;
    [application run];
  }
  return 0;
}
