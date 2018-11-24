#import <AppKit/AppKit.h>

#import "NextApplicationDelegate.h"
#import "NextApplication.h"

int main(int argc, const char * argv[])
{
    NextApplication *app = [NextApplication sharedApplication];
    [app setDelegate:[[NextApplicationDelegate alloc] init]];
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];
    [app run];
    
    return 0;
}
