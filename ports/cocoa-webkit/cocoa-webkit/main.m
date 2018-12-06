#import <AppKit/AppKit.h>

#import "NextApplicationDelegate.h"
#import "NextApplication.h"

int main(int argc, const char * argv[])
{
    NSArray *arguments = NSProcessInfo.processInfo.arguments;
//  NSLog(@"%@", arguments);

    if ([arguments containsObject:@"-h"] || [arguments containsObject:@"--help"])
    {
        printf("%s\n", [@"Help String!" UTF8String]);
    }

    NextApplication *app = [NextApplication sharedApplication];
    [app setDelegate:[[NextApplicationDelegate alloc] init]];
    [app setActivationPolicy:NSApplicationActivationPolicyRegular];
    [app run];

    return 0;
}
