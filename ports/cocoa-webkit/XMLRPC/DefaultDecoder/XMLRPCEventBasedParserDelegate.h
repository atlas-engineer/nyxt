#import "XMLRPCDecoder.h"
#import "XMLRPCElementDecoder.h"
#import <Foundation/Foundation.h>

@interface XMLRPCEventBasedParserDelegate : NSObject <NSXMLParserDelegate> {
    // Without ARC this reference is effectively unretained so don't use strong reference here.
    XMLRPCEventBasedParserDelegate* __unsafe_unretained myParent;
    NSMutableSet* myChildren;
    XMLRPCElementType myElementType;
    NSString* myElementKey;
    id myElementValue;
}

- (id)initWithParent:(XMLRPCEventBasedParserDelegate*)parent;

- (void)setParent:(XMLRPCEventBasedParserDelegate*)parent;

- (XMLRPCEventBasedParserDelegate*)parent;

- (void)setElementType:(XMLRPCElementType)elementType;

- (XMLRPCElementType)elementType;

- (void)setElementKey:(NSString*)elementKey;

- (NSString*)elementKey;

- (void)setElementValue:(id)elementValue;

- (id)elementValue;

@end
