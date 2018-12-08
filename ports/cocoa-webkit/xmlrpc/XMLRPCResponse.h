#import <Foundation/Foundation.h>

@class XMLRPCDecoder;

@interface XMLRPCResponse : NSObject {
    NSString *myBody;
    id myObject;
    BOOL isFault;
}

- (id)initWithData: (NSData *)data;

- (BOOL)isFault;

- (NSNumber *)faultCode;

- (NSString *)faultString;

- (id)object;

- (NSString *)body;

- (NSString *)description;

@end
